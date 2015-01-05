-module(xrel_provider_release).
-behaviour(xrel_provider).
-include("../include/xrel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, release).

init(State) ->
  xrel_config:add_provider(
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
  AllApps = xrel_release:resolv_apps(State),
  _ = xrel_release:make_root(State),
  _ = xrel_release:make_lib(State, AllApps),
  _ = xrel_release:make_release(State, AllApps),
  _ = xrel_release:make_bin(State),
  _ = xrel_release:include_erts(State),
  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State.

