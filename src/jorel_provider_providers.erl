% @hidden
-module(jorel_provider_providers).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, providers).

init(State) ->
  jorel_config:add_provider(
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
  {providers_def, Providers} = jorel_config:get(State, providers_def),
  lists:foreach(fun({Name, #{depends := Deps, desc := Desc}}) ->
                    case Deps of
                      [] ->
                        io:format("~s: ~s~n", [Name, Desc]);
                      Deps ->
                        io:format("~s: ~s (dependencies: ~p)~n", [Name, Desc, Deps])
                    end
                end, Providers),
  State.

