-module(spreadEr).

-export([apply/1]).
-export([subscribe/0,
         sync/0,
         contact/0,
         wait_for_nodes/0,
         master_node/0,
         mnesia_setup/0]).

-include("tables.hrl").

-define(DEFAULT_CLUSTER_SIZE, 1).

%% ===================================================================
%% API
%% ===================================================================

subscribe() ->
    ok = resource_discovery:add_local_resource_tuple({?MODULE, node()}),
    ok = resource_discovery:add_target_resource_type(?MODULE).

sync() ->
    resource_discovery:sync_resources().

contact() ->
    resource_discovery:contact_nodes().

wait_for_nodes() ->
    case master_node() == node() of
        false ->
            ok; %% new node joining cluster, no need to wait.
        true ->
            wait_for(get_env(spreadEr, cluster_size, ?DEFAULT_CLUSTER_SIZE))
    end.

master_node() ->
    [MasterNode] = get_env(resource_discovery, contact_nodes, ['nonode']),
    MasterNode.

mnesia_setup() ->
    Nodes = resource_discovery:get_resources(?MODULE),
    {_, []} = rpc:multicall(Nodes, application, stop, [mnesia]),
    mnesia:create_schema(Nodes),
    {_, []} = rpc:multicall(Nodes, application, start, [mnesia]),
    [ mnesia:create_table(Name, Def) || {Name, Def} <- ?TABLES ],
    mnesia:wait_for_tables([Name || {Name, _} <- ?TABLES], 20000).

apply(Action) ->
    lager:log(info, "Starting ~p action", [Action]),
    ?MODULE:Action().

%% ===================================================================
%% Internal
%% ===================================================================

wait_for(1) ->
    ok;
wait_for(N) ->
    case resource_discovery:get_num_resource(?MODULE) of
        N ->
            ok;
        _ ->
            timer:sleep(5000),
            sync(),
            wait_for(N)
    end.


get_env(App, Var, Def) ->
    case application:get_env(App, Var) of
        undefined ->
            Def;
        {ok, Val} ->
            Val
    end.




















