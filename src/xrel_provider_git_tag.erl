-module(xrel_provider_git_tag).
-behaviour(xrel_provider).
-include("../include/xrel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, 'git-tag').

init(State) ->
  xrel_config:add_provider(
    State,
    {?PROVIDER,
     #{
       module => ?MODULE,
       depends => [],
       desc => "Create a git tag"
      }
    }
   ).

do(State) ->
  ?INFO("== Start provider ~p", [?PROVIDER]),
  Uncommit = uncommit(),
  _ = if
    length(Uncommit) > 0 -> ?HALT("You have uncommit changes ~p, please commit first", [Uncommit]);
    true -> ok
  end,
  {relvsn, RelVsn} = xrel_config:get(State, relvsn),
  {ok, #{v := Version}} = vsn:parse(RelVsn),
  LastTag = last_tag(),
  NextTag = case vsn:compare(Version, LastTag) of
              -1 ->
                {ok, Next} = vsn:bump(patch, LastTag),
                Next;
              _ ->
                Version
            end,
  TagVersion = case ?ASK("Tag version ? [~s]", [NextTag], ": ") of
                 [] -> NextTag;
                 V -> V
               end,
  _ = update_xrel(TagVersion),
  RebarFileForTag = create_rebar_release(TagVersion),
  ?REMARK("== Applications version...", []),
  AppFiles = create_app_release(),
  ok = file:rename("rebar.config", "rebar.config.save"),
  ok = file:rename(RebarFileForTag, "rebar.config"),
  {ok, _} = git_add(["rebar.config", "xrel.config"|AppFiles]),
  {ok, _} = git_commit("Prepare version " ++ TagVersion),
  {ok, _} = git_tag(TagVersion),
  ok = file:rename("rebar.config.save", "rebar.config"),
  AppFiles2 = create_app_next_release(AppFiles),
  {ok, NextVersion} = vsn:bump(patch, TagVersion),
  NextVersion1 = case ?ASK("Next version ? [~s]", [NextVersion], ": ") of
                   [] -> NextVersion;
                   V1 -> V1
                 end,
  {ok, #{v := NextVersion2}} = vsn:parse(NextVersion1),
  NextVersion3 = NextVersion2 ++ "-pre",
  _ = update_xrel(NextVersion3),
  {ok, _} = git_add(["rebar.config", "xrel.config"|AppFiles2]),
  {ok, _} = git_commit("Bump version to " ++ NextVersion3),
  ?INFO("== All good, don't forget to `git push --tags`", []),
  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State.

update_xrel(Version) ->
  {ok, Xrel} = erlconf:open(app, "xrel.config", [{save_on_close, true}]),
  Data = erlconf:term(Xrel),
  _ = case lists:keyfind(release, 1, Data) of
        {release, {AppName, _AppVersion}, Apps} ->
          {ok, _} = erlconf:term(Xrel, lists:keyreplace(release, 1, Data, {release, {AppName, Version}, Apps}));
        _ ->
          ?HALT("!! Can't update version in xrel.config", [])
      end,
  close = erlconf:close(Xrel).

create_rebar_release(Version) ->
  {ok, Rebar} = erlconf:open(rebar, "rebar.config", [{save_on_close, false}]),
  RebarData = erlconf:term(Rebar),
  {deps, Deps} = lists:keyfind(deps, 1, RebarData),
  ?REMARK("== Depencies version...", []),
  Deps1 = lists:foldl(fun({Name, Vsn, {Git, GitURL, Branch}}, DepsAcc) ->
                          {CurV, AvailV} = lists:foldl(
                                             fun({BType, BVersion}, {CurVersion, Acc}) ->
                                                 {CurVersion, maps:put(BVersion, eutils:to_atom(BType), Acc)}
                                             end,
                                             if
                                               is_tuple(Branch) ->
                                                 {BType, BVersion} = Branch,
                                                 {BVersion, maps:put(BVersion, eutils:to_atom(BType), #{})};
                                               true ->
                                                 {Branch, maps:put(Branch, branch, #{})}
                                             end,
                                             remote_tags(GitURL)),
                          UseVersion = deps_version(Name, CurV, AvailV),
                          [{Name, Vsn, {Git, GitURL, {maps:get(UseVersion, AvailV), UseVersion}}}|DepsAcc]
                      end, [], Deps),
  {ok, _} = erlconf:term(Rebar, lists:keyreplace(deps, 1, RebarData, {deps, Deps1})),
  RebarFileForTag = "rebar.config." ++ Version,
  ok = erlconf:save(Rebar, RebarFileForTag),
  close = erlconf:close(Rebar),
  RebarFileForTag.

create_app_release() ->
  lists:foldl(fun(AppFile, Acc) ->
                  {ok, App} = erlconf:open(app, AppFile, [{save_on_close, true}]),
                  [{application, AppName, AppData}] = erlconf:term(App),
                  NewAcc = case lists:keyfind(vsn, 1, AppData) of
                             {vsn, AppVersion} ->
                               {ok, #{v := AppRelVersion}} = vsn:parse(AppVersion),
                               RelVersion = case ?ASK("Version for app ~s ? [~s]", [AppName, AppRelVersion], ": ") of
                                              [] -> AppRelVersion;
                                              V1 -> V1
                                            end,
                               if
                                 RelVersion =:= AppVersion ->
                                   Acc;
                                 true ->
                                   {ok, _} = erlconf:term(App, [{application, AppName, lists:keyreplace(vsn, 1, AppData, {vsn, RelVersion})}]),
                                   [AppFile|Acc]
                               end;
                             _ ->
                               ?ERROR("!! Can't find version for app ~s", [AppName]),
                               Acc
                           end,
                  close = erlconf:close(App),
                  NewAcc
              end, [], app_files()).

create_app_next_release(AppFiles) ->
  lists:foldl(fun(AppFile, Acc) ->
                  {ok, App} = erlconf:open(app, AppFile, [{save_on_close, true}]),
                  [{application, AppName, AppData}] = erlconf:term(App),
                  NewAcc = case lists:keyfind(vsn, 1, AppData) of
                             {vsn, AppVersion} ->
                               {ok, NextAppVersion} = vsn:bump(patch, AppVersion),
                               NextAppVersion1 = NextAppVersion ++ "-pre",
                               {ok, _} = erlconf:term(App, [{application, AppName, lists:keyreplace(vsn, 1, AppData, {vsn, NextAppVersion1})}]),
                               [AppFile|Acc];
                             _ ->
                               ?ERROR("!! Can't find version for App ~s", [AppName]),
                               Acc
                           end,
                  close = erlconf:close(App),
                  NewAcc
              end, [], AppFiles).

deps_version(Name, Version, AvailableVersions) ->
  Versions = maps:keys(AvailableVersions),
  if
    Versions =:= [Version] -> Version;
    true ->
      ?INFO("== Availables versions for ~s : ~p", [Name, Versions]),
      case ?ASK("Version for dependency ~s ? [~s]", [Name, Version], ": ") of
        [] -> Version;
        V ->
          case elists:include(Versions, V) of
            true -> V;
            false ->
              ?ERROR("!! Invalid version ~p for ~s. Use ~p", [V, Name, Versions]),
              deps_version(Name, Version, AvailableVersions)
          end
      end
  end.

last_tag() ->
  lists:foldl(fun(V, Result) ->
                  if
                    Result =:= "" -> V;
                    true ->
                      Compare = vsn:compare(Result, V),
                      if
                        Compare >= 0 -> Result;
                        true -> V
                      end
                  end
              end, "", string:tokens(oksh("git tag"), [10, 13])).

remote_tags(URL) ->
  [ref(T) || T <- string:tokens(oksh("git ls-remote --tags " ++ URL), [10, 13])].
ref(T) ->
  [_, R] = string:tokens(T, "\t "),
  ["refs", B|V] = string:tokens(R, "/"),
  B1 = if
         B =:= "tags" -> "tag";
         true -> B
       end,
  {B1, string:join(V, "/")}.

sh(Cmd, Opts) ->
  sh:sh(Cmd, [{use_stdout, false}, return_on_error] ++ Opts).
sh(Cmd, Args, Opts) ->
  sh:sh(Cmd, Args, [{use_stdout, false}, return_on_error] ++ Opts).

oksh(Cmd) ->
  oksh(Cmd, []).
oksh(Cmd, Opts) ->
  {ok, Rep} = sh(Cmd, Opts),
  Rep.
%oksh(Cmd, Args, Opts) ->
%  {ok, Rep} = sh(Cmd, Args, Opts),
%  Rep.

uncommit() ->
  [F || [X, Y, _|F] <- string:tokens(oksh("git status --porcelain"), [10, 13]), X =:= $M orelse Y =:= $M].

git_add(Files) ->
  sh("git add ~s", [string:join(Files, " ")], []).

git_commit(Comment) ->
  sh("git commit -m \"~s\"", [Comment], []).

git_tag(Version) ->
  sh("git tag ~s", [Version], []).

app_files() ->
  AppPaths = filelib:wildcard("{src,apps,ebin}/**/*.{app,app.src}"),
  AppFiles = lists:map(fun filename:basename/1, AppPaths),
  lists:foldl(fun(App, Acc) ->
                  case filename:extension(App) of
                    ".app" ->
                      case elists:include(AppFiles, filename:basename(App) ++ ".src") of
                        true -> Acc;
                        false -> [App|Acc]
                      end;
                    ".src" ->
                      [App|Acc];
                    _ ->
                      Acc
                  end
              end, [], AppPaths).

