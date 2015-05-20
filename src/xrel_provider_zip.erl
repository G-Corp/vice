-module(xrel_provider_zip).
-behaviour(xrel_provider).
-include("../include/xrel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, zip).

init(State) ->
  xrel_config:add_provider(
    State,
    {?PROVIDER,
     #{
       module => ?MODULE,
       depends => [release],
       desc => "Create an zip archive"
      }
    }
   ).

do(State) ->
  ?INFO("== Start provider ~p", [?PROVIDER]),
  {output_dir, Outdir} = xrel_config:get(State, output_dir),
  {relname, RelName} = xrel_config:get(State, relname),
  {relvsn, RelVsn} = xrel_config:get(State, relvsn),
  ZipFile = eutils:to_list(RelName) ++ "-" ++ RelVsn ++ ".zip",
  eos:in(Outdir, fun() ->
                     ?INFO("Create ~s", [ZipFile]),
                     zip:create(ZipFile, [eutils:to_list(RelName)])
                 end),
  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State.

