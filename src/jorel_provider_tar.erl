% @hidden
-module(jorel_provider_tar).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, tar).

init(State) ->
  jorel_config:add_provider(
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
  {output_dir, Outdir} = jorel_config:get(State, output_dir),
  {relname, RelName} = jorel_config:get(State, relname),
  {relvsn, RelVsn} = jorel_config:get(State, relvsn),
  TarFile = eutils:to_list(RelName) ++ "-" ++ RelVsn ++ ".tar.gz",
  eos:in(Outdir, fun() ->
                     ?INFO("Create ~s", [TarFile]),
                     erl_tar:create(TarFile, [eutils:to_list(RelName)], [compressed])
                 end),
  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State.

