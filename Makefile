.PHONY: doc docker-compose.yml
REBAR = ./rebar3

compile:
	$(verbose) $(REBAR) escriptize

tests:
	$(verbose) $(REBAR) eunit

doc:
	$(verbose) $(REBAR) as doc edoc

dist: compile tests doc

distclean:
	$(verbose) rm -rf _build rebar.lock test/eunit

dev: compile
	$(verbose) erl -pa _build/default/lib/*/ebin _build/default/lib/*/include

