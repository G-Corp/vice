REBAR   = ./rebar
XREL    = ./xrel
ELVIS   = ./elvis
VERSION = $(shell ./tag)

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
	@rm -rf deps
	@rm -rf ebin

test: compile
	@$(REBAR) skip_deps=true eunit

doc:
	$(REBAR) skip_deps=true doc

dev:
	@erl -pa ebin include deps/*/ebin deps/*/include

escript: compile
	@$(REBAR) skip_deps=true escriptize

elvis: compile
	@$(ELVIS) rock

release: escript
ifeq ($(VERSION),ERROR)
	@echo "**> Can't find version!"
else
	@echo "==> Release version $(VERSION)"
	git clone git@github.com:emedia-project/xrel.wiki.git
	cp xrel xrel.wiki/xrel
	cd xrel.wiki; git commit -am "New release $(VERSION)"; git push origin master
	rm -rf xrel.wiki
endif

