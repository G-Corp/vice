% @hidden
-module(jorel_provider_release).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, release).

init(State) ->
  jorel_config:add_provider(
    State,
    {?PROVIDER,
     #{
       module => ?MODULE,
       depends => [],
       desc => "Create a release"
      }
    }
   ).

do(State) ->
  ?INFO("== Start provider ~p", [?PROVIDER]),
  ?INFO("= Elixir project: ~p", [jorel_elixir:exist()]),
  _ = jorel_release:get_erts(State),
  AllApps = jorel_release:resolv_apps(State),
  BootApps = jorel_release:resolv_boot(State, AllApps),
  _ = jorel_release:make_root(State),
  _ = jorel_release:make_lib(State, AllApps),
  _ = jorel_release:make_release(State, AllApps, BootApps),
  _ = jorel_release:make_boot_script(State, BootApps),
  _ = jorel_release:make_bin(State),
  _ = jorel_release:make_relup(State, AllApps),
  _ = jorel_release:include_erts(State),
  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State.

