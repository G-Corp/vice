PROJECT = jorel_sample
PROJECT_DESCRIPTION = Sample projet
PROJECT_VERSION = 0.0.1

DEP_PLUGINS = jorel
REL_DEPS = jorel

dep_jorel = git https://github.com/emedia-project/jorel.git master

include erlang.mk

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include

