% @hidden
-module(jorel_provider_appup).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, appup).

init(State) ->
  jorel_config:add_provider(
    State,
    {?PROVIDER,
     #{
       module => ?MODULE,
       depends => [],
       desc => "Generate appup file"
      }
    }
   ).

do(State) ->
  ?INFO("== Start provider ~p", [?PROVIDER]),
  {relname, RelName} = jorel_config:get(State, relname),
  {relvsn, RelVsn} = jorel_config:get(State, relvsn),
  {outdir, Outdir} = jorel_config:get(State, outdir),
  RelDir = filename:join([Outdir, "releases"]),
  RelNameAtom = bucs:to_string(RelName),
  ?INFO("= Current release ~s (version ~s)", [RelName, RelVsn]),
  ReleaseApps = [{App, Vsn, Path} || #{app := App,
                                       vsn := Vsn,
                                       path := Path} <- jorel_release:resolv_apps(State)],
  ?INFO("= Search for previous releases in ~s", [RelDir]),
  Rels = lists:foldl(fun(DepsFile, Acc) ->
                         case file:consult(DepsFile) of
                           {ok, [{release, {RelNameAtom, Vsn}, _, Deps}]} ->
                             case vsn:compare(RelVsn, Vsn) of
                               1 -> 
                                 ?INFO("= Found release ~s", [Vsn]),
                                 [{Vsn, Deps}|Acc];
                               _ ->
                                 ?DEBUG("= Ignore release ~s (>= ~s)", [Vsn, RelVsn]),
                                 Acc
                             end;
                           {ok, [{release, {Name, Vsn}, _, _}]} ->
                             ?DEBUG("= Ignore ~s (version ~s): name mismatch", [Name, Vsn]),
                             Acc;
                           {error, _} ->
                             Acc
                         end
                     end, [], filelib:wildcard(
                                filename:join(
                                  [RelDir, "*", "*.deps"]))),
  case Rels of
    [] ->
      ?ERROR("= No previous release found", []);
    _ ->
      AppDiffs = [{Vsn, diff_apps(Deps, ReleaseApps)} || {Vsn, Deps} <- Rels],
      ?DEBUG("====> ~p", [AppDiffs])
  end,
  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State.

diff_apps(Deps, ReleaseApps) ->
  Remove = app_diff(Deps, ReleaseApps, 1),
  Add = app_diff(ReleaseApps, Deps, 1),
  Update = lists:foldl(fun({Name, Vsn, Path}, Acc) ->
                           case lists:member({Name, Vsn}, Deps) of
                             true ->
                               Acc;
                             false ->
                               [{Name, Vsn, Path}|Acc]
                           end
                       end, [], ReleaseApps),
  {Remove, Add, Update}.

app_diff(List1, List2, N) ->
  lists:foldl(fun(E, List) ->
                  lists:keydelete(element(N, E), N, List)
              end, List1, List2).

