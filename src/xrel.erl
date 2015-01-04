-module(xrel).
-include("../include/xrel.hrl").

-export([main/1]).

main(Args) ->
  case getopt:parse(opts(), Args) of
    {ok, {Options, _NonOptions}} ->
      case lists:member(version, Options) of
        true ->
          application:load(xrel),
          {ok, Vsn} = application:get_key(xrel, vsn),
          io:format("xrel ~s~n", [Vsn]);
        false ->
          case lists:member(help, Options) of
            true ->
              help();
            false ->
              run(Options)
          end
      end;
    {error, Details} ->
      io:format("Error : ~p~n", [Details])
  end.

run(Options) ->
  ?INFO("== Start xrel", []),
  State = xrel_config:to_state(Options),
  AllApps = xrel_release:resolv_apps(State),
  _ = xrel_release:make_root(State),
  _ = xrel_release:make_lib(State, AllApps),
  _ = xrel_release:make_release(State, AllApps),
  _ = xrel_release:make_bin(State),
  _ = xrel_release:include_erts(State),
  ?INFO("== Done", []),
  ok.

opts() ->
  [
   {relname,      $n,        "relname",      undefined,               "Specify the name for the release that will be generated"},
   {relvsn,       $v,        "relvsn",       undefined,               "Specify the version for the release"},
   {config,       $c,        "config",       {string, "xrel.config"}, "Path to the config file"},
   {help,         $h,        "help",         undefined,               "Display this help"},
   {version,      $V,        "version",      undefined,               "Display version"},
   {output_dir,   $o,        "output-dir",   {string, "./_xrel"},     "Output directory"},
   {exclude_path, $e,        "exclude-path", {list, ["_xrel"]},       "Exclude directories"},
   {include_src,  undefined, "include-src",  undefined,               "Include source"}
  ].

help() ->
  getopt:usage(opts(), "xrel", "").
