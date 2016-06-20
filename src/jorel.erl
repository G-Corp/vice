-module(jorel).
-include("../include/jorel.hrl").

-export([main/1]).
-export([run/2]).

% @hidden
main(Args) ->
  case getopt:parse(opts(), Args) of
    {ok, {Options, Commands}} ->
      case lists:member(version, Options) of
        true ->
          application:load(jorel),
          {ok, Vsn} = application:get_key(jorel, vsn),
          io:format("jorel ~s~n", [Vsn]);
        false ->
          case lists:member(help, Options) of
            true ->
              help(),
              io:format("~nCommandes:~n~n"),
              run(Options, [providers]);
            false ->
              run(Options, Commands)
          end
      end;
    {error, Details} ->
      io:format("Error : ~p~n", [Details])
  end.

run(Options, Commands) ->
  State = jorel_config:to_state(Options, Commands),
  {config, JorelConfig} = jorel_config:get(State, config),
  {providers, Providers} = jorel_config:get(State, providers, []),
  {paths, Paths} = jorel_config:get(State, paths, []),
  load_paths(Paths),
  {State1, Providers1} = add_provider(State, Providers, jorel_provider_release),
  {State2, Providers2} = add_provider(State1, Providers1, jorel_provider_providers),
  {State3, Providers3} = add_provider(State2, Providers2, jorel_provider_config),
  State4 = lists:foldl(fun(P, S) ->
                           load_provider(P, S)
                       end, State3, Providers3),
  Commands2 = case lists:map(fun bucs:to_atom/1, Commands) of
                [] -> [release];
                Commands1 -> Commands1
              end,
  _ = lists:foldl(fun(P, S) ->
                      if
                        P =/= providers ->
                          ?INFO("== Config file: ~s", [JorelConfig]);
                        true ->
                          ok
                      end,
                      jorel_provider:run(S, P)
                  end, State4, Commands2),
  ok.

add_provider(State, Providers, Provider) ->
  case lists:member(Provider, Providers) of
    true ->
      {State, Providers};
    false ->
      {jorel_config:set(State, {providers, [Provider|Providers]}),
       [Provider|Providers]}
  end.

load_provider(Provider, State) ->
  _ = application:load(Provider),
  case code:ensure_loaded(Provider) of
    {module, Module} ->
      case erlang:function_exported(Module, init, 1) of
        true ->
          erlang:apply(Module, init, [State]);
        false ->
          ?ERROR("Provider ~p does not export init/1. It will not be used.", [Provider]),
          State
      end;
    {error, _} ->
      ?ERROR("Provider ~p not found. It will not be used.", [Provider]),
      State
  end.

load_paths(Paths) ->
  code:add_pathsa(load_paths(Paths, [])).
load_paths([], Acc) ->
  Acc;
load_paths([Path|Rest], Acc) ->
  Ebins = case filelib:wildcard(Path) of
            [Path] -> 
              Ebin = filename:join([Path, "ebin"]),
              case filelib:is_dir(Ebin) of
                true ->
                  [Ebin];
                false ->
                  []
              end;
            Paths ->
              load_paths(Paths, [])
          end,
  load_paths(Rest, Acc ++ Ebins).

opts() ->
  [
   {relname,      $n,        "relname",      string,                   "Specify the name for the release that will be generated"},
   {relvsn,       $v,        "relvsn",       string,                   "Specify the version for the release"},
   {config,       $c,        "config",       {string, "jorel.config"}, "Path to the config file"},
   {help,         $h,        "help",         undefined,                "Display this help"},
   {version,      $V,        "version",      undefined,                "Display version"},
   {output_dir,   $o,        "output-dir",   {string, "./_jorel"},     "Output directory"},
   {exclude_dirs, $e,        "exclude-dirs", list,                     "Exclude directories"},
   {include_src,  undefined, "include-src",  undefined,                "Include source"}
  ].

help() ->
  getopt:usage(opts(), "jorel", "").
