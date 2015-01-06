REBAR = ./rebar
RELX = ./xrel

.PHONY: compile get-deps test

all: escript

compile: get-deps
	@$(REBAR) compile

get-deps:
	@$(REBAR) get-deps
	@$(REBAR) check-deps

clean:
	@$(REBAR) clean
	@rm -f erl_crash.dump
	@rm -f priv/templates/*.dtl.erl
	@rm -f xrel

realclean: clean
	@$(REBAR) delete-deps

test: compile
	@$(REBAR) skip_deps=true eunit

doc:
	$(REBAR) skip_deps=true doc

dev:
	@erl -pa ebin include deps/*/ebin deps/*/include

escript: compile
	@$(REBAR) skip_deps=true escriptize

release: escript
	@$(XREL)

