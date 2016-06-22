% @hidden
-module(jorel_provider_relup).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, relup).

init(State) ->
  jorel_config:add_provider(
    State,
    {?PROVIDER,
     #{
       module => ?MODULE,
       depends => [],
       desc => "Create relup of release"
      }
    }
   ).

do(State) ->
  ?INFO("== Start provider ~p", [?PROVIDER]),
  ERTSInfo = jorel_release:get_erts(State),
  State1 = jorel_config:set(State, {erts_info, ERTSInfo}),
  AllApps = jorel_release:resolv_apps(State1), % TODO Remove and use deps file in release
  _ = jorel_release:make_relup(State1, AllApps),
  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State1.

