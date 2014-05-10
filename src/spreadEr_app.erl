-module(spreadEr_app).

-behaviour(application).

%% API
-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% API
%% ===================================================================

start() ->
    [ ok = application:start(App) || App <- [mnesia, 
                                             resource_discovery, 
                                             spreadEr] ].

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

    Actions = [subscribe, contact, sync],
    [ ok = erlang:apply(spreadEr, Action, []) || Action <- Actions ],
    
    MasterActions = [wait_for_nodes, mnesia_setup],
    [ ok = erlang:apply(spreadEr, Action, []) || Action <- MasterActions,
                                                 node() == spreadEr:master_node() ],

    spreadEr_sup:start_link().

stop(_State) ->
    ok.

