# Erlang data spreader #

spreadEr is an Erlang library that  makes it easy to spin off a mnesia-based storage solution across different nodes.

== Configuration ==

* Define your own tables in `tables.hrl`
* Define contact_nodes variable of `resource_discovery` application.
* Define cluster_size variable of `spreadEr` application.

See `priv/*.config` files for examples.