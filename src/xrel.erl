-module(xrel).
-include("../include/xrel.hrl").

-export([main/1]).

main(Args) ->
  case getopt:parse(opts(), Args) of
    {ok, {Options, Commands}} ->
      case lists:member(version, Options) of
        true ->
          application:load(xrel),
          {ok, Vsn} = application:get_key(xrel, vsn),
          io:format("xrel ~s~n", [Vsn]);
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
  State = xrel_config:to_state(Options),
  {providers, Providers} = xrel_config:get(State, providers, []),
  {State1, Providers1} = case elists:include(Providers, release) of
                           true ->
                             {State, Providers};
                           false ->
                             {xrel_config:set(State, {providers, [xrel_provider_release|Providers]}),
                              [xrel_provider_release, xrel_provider_providers|Providers]}
                         end,
  State2 = lists:foldl(fun(P, S) ->
                           P:init(S)
                       end, State1, Providers1),
  Commands2 = case lists:map(fun eutils:to_atom/1, Commands) of
                [] -> [release];
                Commands1 -> Commands1
              end,
  _ = lists:foldl(fun(P, S) ->
                      xrel_provider:run(S, P)
                  end, State2, Commands2),
  ok.

opts() ->
  [
   {relname,      $n,        "relname",      undefined,               "Specify the name for the release that will be generated"},
   {relvsn,       $v,        "relvsn",       undefined,               "Specify the version for the release"},
   {config,       $c,        "config",       {string, "xrel.config"}, "Path to the config file"},
   {help,         $h,        "help",         undefined,               "Display this help"},
   {version,      $V,        "version",      undefined,               "Display version"},
   {output_dir,   $o,        "output-dir",   {string, "./_xrel"},     "Output directory"},
   {exclude_dirs, $e,        "exclude-dirs", {list, ["_xrel"]},       "Exclude directories"},
   {include_src,  undefined, "include-src",  undefined,               "Include source"}
  ].

help() ->
  getopt:usage(opts(), "xrel", "").
