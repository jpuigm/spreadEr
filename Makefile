# -*- mode: Makefile; fill-column: 80; comment-column: 75; -*-

ERL = $(shell which erl)

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/*/*/ebin

REBAR=rebar

all: clean compile

compile: get-deps
	@$(REBAR) compile

clean: 
	@$(REBAR) clean
	rm -rf Mnesia* *~ erl_crash.dump

get-deps:
	@$(REBAR) get-deps

single: 
	@$(ERL) -name spreadEr_single@127.0.0.1 $(ERLFLAGS) -s spreadEr_app

c2_master:
	@$(ERL) -name spreadEr_c2_m@127.0.0.1 $(ERLFLAGS) -s spreadEr_app -config 2_nodes_cluster

c2_slave:
	@$(ERL) -name spreadEr_c2_s@127.0.0.1 $(ERLFLAGS) -s spreadEr_app -config 2_nodes_cluster
