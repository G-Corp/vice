-module(jorel).
-include("../include/jorel.hrl").

-export([main/1]).

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
  {providers, Providers} = jorel_config:get(State, providers, []),
  {State1, Providers1} = add_provider(State, Providers, jorel_provider_release),
  {State2, Providers2} = add_provider(State1, Providers1, jorel_provider_providers),
  {State3, Providers3} = add_provider(State2, Providers2, jorel_provider_config),
  State4 = lists:foldl(fun(P, S) ->
                           P:init(S)
                       end, State3, Providers3),
  Commands2 = case lists:map(fun eutils:to_atom/1, Commands) of
                [] -> [release];
                Commands1 -> Commands1
              end,
  _ = lists:foldl(fun(P, S) ->
                      jorel_provider:run(S, P)
                  end, State4, Commands2),
  ok.

add_provider(State, Providers, Provider) ->
  case elists:include(Providers, Provider) of
    true ->
      {State, Providers};
    false ->
      {jorel_config:set(State, {providers, [Provider|Providers]}),
       [Provider|Providers]}
  end.

opts() ->
  [
   {relname,      $n,        "relname",      string,                   "Specify the name for the release that will be generated"},
   {relvsn,       $v,        "relvsn",       string,                   "Specify the version for the release"},
   {config,       $c,        "config",       {string, "jorel.config"}, "Path to the config file"},
   {help,         $h,        "help",         undefined,                "Display this help"},
   {version,      $V,        "version",      undefined,                "Display version"},
   {output_dir,   $o,        "output-dir",   {string, "./_jorel"},     "Output directory"},
   {exclude_dirs, $e,        "exclude-dirs", {list, ["_jorel"]},       "Exclude directories"},
   {include_src,  undefined, "include-src",  undefined,                "Include source"}
  ].

help() ->
  getopt:usage(opts(), "jorel", "").
