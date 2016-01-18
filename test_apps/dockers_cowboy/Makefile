PROJECT = dockers_cowboy
PROJECT_DESCRIPTION = Cowboy Hello World example
PROJECT_VERSION = 1

DEPS = cowboy uuid
dep_cowboy = git https://github.com/ninenines/cowboy.git master
dep_uuid = git https://github.com/avtobiff/erlang-uuid.git master

DEP_PLUGINS = jorel
REL_DEPS = jorel
JOREL_MASTER = true

dep_jorel = git https://github.com/emedia-project/jorel.git master

include erlang.mk

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include

