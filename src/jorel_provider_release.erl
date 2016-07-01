% @hidden
-module(jorel_provider_release).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, release).

init(State) ->
  jorel_config:add_provider(
    State,
    {?PROVIDER,
     #{
       module => ?MODULE,
       depends => [],
       desc => "Create a release",
       opts => [
                {relname,      $n,        "relname",      string,                   "Specify the name for the release that will be generated"},
                {relvsn,       $v,        "relvsn",       string,                   "Specify the version for the release"},
                {output_dir,   $o,        "output-dir",   {string, "./_jorel"},     "Output directory"},
                {exclude_dirs, $e,        "exclude-dirs", list,                     "Exclude directories"},
                {include_src,  undefined, "include-src",  undefined,                "Include source"}
               ]
      }
    }
   ).

do(State) ->
  ?INFO("== Start provider ~p", [?PROVIDER]),
  ?INFO("= Elixir project: ~p", [jorel_elixir:exist()]),
  ERTSInfo = jorel_release:get_erts(State),
  State1 = jorel_config:set(State, {erts_info, ERTSInfo}),
  AllApps = jorel_release:resolv_apps(State1),
  BootApps = jorel_release:resolv_boot(State1, AllApps),
  _ = jorel_release:make_root(State1),
  _ = jorel_release:make_lib(State1, AllApps),
  _ = jorel_release:make_release(State1, AllApps, BootApps),
  _ = jorel_release:make_bin(State1),
  _ = jorel_release:include_erts(State1),
  _ = jorel_release:make_upgrade_scripts(State1),
  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State1.

