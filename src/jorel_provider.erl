% @hidden
-module(jorel_provider).
-include("../include/jorel.hrl").

-export([
         run/2,
         run_deps/2,
         has_run/2,
         terminate/2
        ]).

-callback init(any()) -> any().
-callback do(any()) -> any().

run(State, Provider) ->
  {providers_def, Providers} = jorel_config:get(State, providers_def),
  case lists:keyfind(Provider, 1, Providers) of
    {Provider, #{module := ProviderMod}} ->
      run_provider(State, Provider, ProviderMod);
    false ->
      ?HALT("!!! Failed to run provider ~p", [Provider])
  end.

run_deps(State, Provider) ->
  {providers_def, Providers} = jorel_config:get(State, providers_def),
  case lists:keyfind(Provider, 1, Providers) of
    {Provider, #{depends := ProviderDeps}} ->
      lists:foldl(fun(Deps, S) ->
                      run(S, Deps)
                  end, State, ProviderDeps);
    _ ->
      ?HALT("!!! Failed to run provider ~p", [Provider])
  end.

run_provider(State, Provider, ProviderMod) ->
  State1 = jorel_provider:run_deps(State, Provider),
  case jorel_provider:has_run(State1, Provider) of
    true ->
      State1;
    false ->
      State2 = ProviderMod:do(State1),
      jorel_provider:terminate(State2, Provider)
  end.

has_run(State, Provider) ->
  {run, Run} = jorel_config:get(State, run, []),
  lists:member(Provider, Run).

terminate(State, Provider) ->
  {run, Run} = jorel_config:get(State, run, []),
  jorel_config:set(State, {run, [Provider|Run]}).

