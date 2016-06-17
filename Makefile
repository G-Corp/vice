REBAR = ./rebar3

compile:
	@$(REBAR) escriptize

tests:
	@$(REBAR) eunit

doc:
	@$(REBAR) as doc edoc

dist: compile tests doc

distclean:
	@rm -rf _build _jorel rebar.lock test/eunit

dev: compile
	@erl -pa _build/default/lib/*/ebin _build/default/lib/*/include

