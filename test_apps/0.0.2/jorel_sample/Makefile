PROJECT = jorel_sample
PROJECT_DESCRIPTION = Sample projet 
PROJECT_VERSION = 0.0.2

DEP_PLUGINS = jorel
REL_DEPS = jorel

dep_jorel = git https://github.com/emedia-project/jorel.git master

include erlang.mk

app::
	$(verbose) cp src/$(PROJECT).appup.src ebin/$(PROJECT).appup

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include

