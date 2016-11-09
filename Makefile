include bu.mk

.PHONY: doc

compile:
	$(verbose) $(REBAR) escriptize

tests: compile
	$(verbose) $(REBAR) eunit

doc:
	$(verbose) $(REBAR) edoc

lint:
	$(verbose) $(REBAR) lint

dist: compile tests lint doc

clean:
	$(verbose) $(RM_RF) _build _jorel test/eunit

distclean: clean
	$(verbose) $(RM_F) rebar.lock

dev: compile
	$(verbose) erl -pa _build/default/lib/*/ebin _build/default/lib/*/include

VERSION = $(shell ./tag)
release: compile
ifeq ($(VERSION),ERROR)
	$(verbose) echo "**> Can't find version!"
else
	$(verbose) echo "==> Release version $(VERSION)"
	git clone git@github.com:emedia-project/jorel.wiki.git
	$(CP) _build/default/bin/jorel jorel.wiki/jorel
	md5sum jorel.wiki/jorel | awk '{print $$1}' > jorel.wiki/jorel.md5
	cd jorel.wiki; git add .; git commit -am "New release $(VERSION)"; git push origin master
	$(RM_RF) jorel.wiki
endif

release-master: compile
	$(version) echo "==> Release master"
	git clone git@github.com:emedia-project/jorel.wiki.git
	$(CP) _build/default/bin/jorel jorel.wiki/jorel.master
	md5sum jorel.wiki/jorel.master | awk '{print $$1}' > jorel.wiki/jorel.master.md5
	cd jorel.wiki; git add .; git commit -am "New master"; git push origin master
	$(RM_RF) jorel.wiki

