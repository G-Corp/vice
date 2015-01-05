-module(xrel_provider_providers).
-behaviour(xrel_provider).
-include("../include/xrel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, providers).

init(State) ->
  xrel_config:add_provider(
    State,
    {?PROVIDER,
     #{
       module => ?MODULE,
       depends => [],
       desc => "List availables providers"
      }
    }
   ).

do(State) ->
  {providers_def, Providers} = xrel_config:get(State, providers_def),
  lists:foreach(fun({Name, #{depends := Deps, desc := Desc}}) ->
                    case Deps of
                      [] ->
                        io:format("~s: ~s~n", [Name, Desc]);
                      Deps ->
                        io:format("~s: ~s (dependencies: ~p)~n", [Name, Desc, Deps])
                    end
                end, Providers),
  State.

