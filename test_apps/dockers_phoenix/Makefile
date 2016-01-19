PROJECT = dockers_phoenix

DEP_PLUGINS = jorel
REL_DEPS = jorel
JOREL_MASTER = true

dep_jorel = git https://github.com/emedia-project/jorel.git master

app:
	$(verbose) mix local.hex --force
	$(verbose) mix local.rebar --force
	$(verbose) mix deps.get
	$(verbose) npm install
	$(verbose) MIX_ENV=prod mix compile
	$(verbose) MIX_ENV=prod mix phoenix.digest

distclean::
	$(verbose) rm -rf _build deps mix.lock node_modules

include erlang.mk

