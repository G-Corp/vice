% @hidden
-module(jorel_provider_install).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, install).

init(State) ->
  jorel_config:add_provider(
    State,
    {?PROVIDER,
     #{
       module => ?MODULE
       , depends => []
       , desc => "Install an application"
       , opts => [
                  {relname, $n, "relname", string, "Specify the name for the release to install"},
                  {relvsn,  $v, "relvsn",  string, "Specify the version for the release to install"},
                  {install_path, $I, "install_path", {string, "."}, "Path to install the application"}
                 ]
      }
    }
   ).

do(State) ->
  % {jorel_in, URL} = jorel_config:get(State, jorel_in, ?JOREL_IN),
  {install_path, Path} = jorel_config:get(State, install_path, "."),
  RelName = case jorel_config:get(State, relname, undefined) of
              {relname, undefined} ->
                ?HALT("!!! Missing release name (use --relname)", []);
              {relname, R} ->
                R
            end,
  {relvsn, RelVsn} = jorel_config:get(State, relvsn, last),
  ?DEBUG("* Install ~s (~s) in ~s", [RelName, RelVsn, bucfile:expand_path(Path)]),
  State.

