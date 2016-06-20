.PHONY: doc

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

VERSION = $(shell ./tag)
release: compile
ifeq ($(VERSION),ERROR)
	@echo "**> Can't find version!"
else
	@echo "==> Release version $(VERSION)"
	git clone git@github.com:emedia-project/jorel.wiki.git
	cp _build/default/bin/jorel jorel.wiki/jorel
	cd jorel.wiki; git add .; git commit -am "New release $(VERSION)"; git push origin master
	rm -rf jorel.wiki
endif

release-master: compile
	@echo "==> Release master"
	git clone git@github.com:emedia-project/jorel.wiki.git
	cp _build/default/bin/jorel jorel.wiki/jorel.master
	cd jorel.wiki; git add .; git commit -am "New master"; git push origin master
	rm -rf jorel.wiki

