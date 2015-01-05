-module(xrel_provider).
-include("../include/xrel.hrl").

-export([
         run/2,
         run_deps/2,
         has_run/2,
         terminate/2
        ]).

-callback init(any()) -> any().
-callback do(any()) -> any().

run(State, Provider) ->
  {providers_def, Providers} = xrel_config:get(State, providers_def),
  case lists:keyfind(Provider, 1, Providers) of
    {Provider, #{module := ProviderMod}} ->
      run_provider(State, Provider, ProviderMod);
    false ->
      ?HALT("!!! Missing configuration for provider ~p", [Provider])
  end.

run_deps(State, Provider) ->
  {providers_def, Providers} = xrel_config:get(State, providers_def),
  case lists:keyfind(Provider, 1, Providers) of
    {Provider, #{depends := ProviderDeps}} ->
      lists:foldl(fun(Deps, S) ->
                      run(S, Deps)
                  end, State, ProviderDeps);
    _ -> 
      ?HALT("!!! Missing configuration for provider ~p", [Provider])
  end.

run_provider(State, Provider, ProviderMod) ->
  State1 = xrel_provider:run_deps(State, Provider),
  case xrel_provider:has_run(State1, Provider) of
    true ->
      State1;
    false ->
      State2 = ProviderMod:do(State1),
      xrel_provider:terminate(State2, Provider)
  end.

has_run(State, Provider) ->
  {run, Run} = xrel_config:get(State, run, []),
  elists:include(Run, Provider).

terminate(State, Provider) ->
  {run, Run} = xrel_config:get(State, run, []),
  xrel_config:set(State, {run, [Provider|Run]}).

