PROJECT = jorel

DEPS = sh erlconf vsn color eutils getopt erlydtl
dep_sh = git https://github.com/gleber/sh.git master
dep_erlconf = git https://github.com/emedia-project/erlconf.git master
dep_vsn = git https://github.com/emedia-project/vsn.git master
dep_color = git https://github.com/julianduque/erlang-color.git master
dep_eutils = git https://github.com/emedia-project/eutils.git master
dep_getopt = git https://github.com/jcomellas/getopt.git master
dep_erlydtl = git https://github.com/erlydtl/erlydtl.git master

include erlang.mk

ESCRIPT_SYS_CONFIG = "config/jorel.config"

VERSION = $(shell ./tag)
release: escript
ifeq ($(VERSION),ERROR)
	@echo "**> Can't find version!"
else
	@echo "==> Release version $(VERSION)"
	git clone git@github.com:emedia-project/jorel.wiki.git
	cp jorel jorel.wiki/jorel
	cd jorel.wiki; git commit -am "New release $(VERSION)"; git push origin master
	rm -rf jorel.wiki
endif

dev:
	@erl -pa ebin include deps/*/ebin deps/*/include

