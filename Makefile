.PHONY: doc

REBAR = ./rebar3

compile:
	@$(REBAR) escriptize

tests: compile
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
	md5sum jorel.wiki/jorel | awk '{print $$1}' > jorel.wiki/jorel.md5
	cd jorel.wiki; git add .; git commit -am "New release $(VERSION)"; git push origin master
	rm -rf jorel.wiki
endif

release-master: compile
	@echo "==> Release master"
	git clone git@github.com:emedia-project/jorel.wiki.git
	cp _build/default/bin/jorel jorel.wiki/jorel.master
	md5sum jorel.wiki/jorel.master | awk '{print $$1}' > jorel.wiki/jorel.master.md5
	cd jorel.wiki; git add .; git commit -am "New master"; git push origin master
	rm -rf jorel.wiki

