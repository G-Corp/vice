% @hidden
-module(jorel_provider_zip).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, zip).

init(State) ->
  jorel_config:add_provider(
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
  {output_dir, Outdir} = jorel_config:get(State, output_dir),
  {relname, RelName} = jorel_config:get(State, relname),
  {relvsn, RelVsn} = jorel_config:get(State, relvsn),
  ZipFile = eutils:to_list(RelName) ++ "-" ++ RelVsn ++ ".zip",
  eos:in(Outdir, fun() ->
                     ?INFO("Create ~s", [ZipFile]),
                     zip:create(ZipFile, [eutils:to_list(RelName)])
                 end),
  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State.

