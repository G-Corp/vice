-module(xrel_provider_tar).
-behaviour(xrel_provider).
-include("../include/xrel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, tar).

init(State) ->
  xrel_config:add_provider(
    State,
    {?PROVIDER,
     #{
       module => ?MODULE,
       depends => [release],
       desc => "Create an tar archive"
      }
    }
   ).

do(State) ->
  ?INFO("== Start provider ~p", [?PROVIDER]),
  {output_dir, Outdir} = xrel_config:get(State, output_dir),
  {relname, RelName} = xrel_config:get(State, relname),
  {relvsn, RelVsn} = xrel_config:get(State, relvsn),
  TarFile = eutils:to_list(RelName) ++ "-" ++ RelVsn ++ ".tar.gz",
  eos:in(Outdir, fun() ->
                     ?INFO("Create ~s", [TarFile]),
                     erl_tar:create(TarFile, [eutils:to_list(RelName)], [compressed])
                 end),
  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State.

