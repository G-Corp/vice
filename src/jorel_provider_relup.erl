% @hidden
-module(jorel_provider_relup).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, relup).

init(State) ->
  jorel_config:add_provider(
    State,
    {?PROVIDER,
     #{
       module => ?MODULE,
       depends => [],
       desc => "Create relup of release"
      }
    }
   ).

do(State) ->
  ?INFO("== Start provider ~p", [?PROVIDER]),
  {relname, RelName} = jorel_config:get(State, relname),
  {relvsn, RelVsn} = jorel_config:get(State, relvsn),
  {outdir, Outdir} = jorel_config:get(State, outdir),
  {upfrom, UpFrom} = jorel_config:get(State, upfrom, "0"),
  NameWithVsn = io_lib:format("~s-~s", [RelName, RelVsn]),

  CurrentRel = filename:join([Outdir, "releases", RelVsn, NameWithVsn ++ ".rel"]),
  {Rel, ErtsVersion, Deps} = case file:consult(CurrentRel) of
                               {ok, [{release, _, {erts, V}, D}]} ->
                                 {strip_rel(CurrentRel), V, D};
                               _ ->
                                 ?HALT("Missing release ~s (version ~s)", [RelName, RelVsn])
                             end,

  ReleasesPaths = get_releases_path(Outdir, UpFrom, RelVsn),
  UpDown = get_up_from(ReleasesPaths, RelName, RelVsn),
  case UpDown of
    [] ->
      ?HALT("No previous ~s release found", [RelName]);
    _ ->
      ok
  end,

  Options = [{outdir, filename:join([Outdir, "releases", RelVsn])},
             {path, get_all_paths(ReleasesPaths, RelName, Outdir)},
             silent],

  ?INFO("* Create relup", []),
  ?DEBUG("= systools:make_relup(~p, ~p, ~p, ~p)", [Rel, UpDown, UpDown, Options]),
  case systools:make_relup(Rel, UpDown, UpDown, Options) of
    {error, _, Error} ->
      ?HALT("!!! Create relup faild: ~p", [Error]);
    error ->
      ?HALT("!!! Create relup faild", []);
    _ ->
      todo
  end,

  ?INFO("* Create release archive", []),
  RelArchive = filename:join([Outdir, "releases", NameWithVsn ++ ".tar.gz"]),
  ?DEBUG("= ~s", [RelArchive]),
  case erl_tar:open(RelArchive, [write, compressed]) of
    {ok, Tar} ->
      % Add libs
      Lib = filename:join([Outdir, "lib"]),
      [add_to_tar(Tar, F, bucfile:relative_from(F, Outdir), []) 
       || F <- lists:concat([filelib:wildcard(filename:join([L, "**", "*"])) 
                             || L <- [filename:join([Lib, io_lib:format("~s-~s", [N, V])]) 
                                      || {N, V} <- Deps]])],
      % Add erts
      case jorel_config:get(State, include_erts, true) of
        {include_erts, false} ->
          ok;
        _ ->
          Erts = filename:join([Outdir, io_lib:format("erts-~s", [ErtsVersion])]),
          [add_to_tar(Tar, F, bucfile:relative_from(F, Outdir), []) ||
           F <- filelib:wildcard(filename:join([Erts, "**", "*"]))]
      end,
      % Add bin
      [add_to_tar(Tar, filename:join([Outdir, "bin", F]), filename:join(["bin", F]), []) ||
       F <- [RelName, NameWithVsn, "nodetool", "start.boot", "start_clean.boot", "upgrade.escript"]],

      % Add release
      Release = filename:join([Outdir, "releases", RelVsn]),
      [add_to_tar(Tar, F, bucfile:relative_from(F, Outdir), []) ||
       F <- filelib:wildcard(filename:join([Release, "**", "*"]))],
      add_to_tar(Tar,
                 filename:join([Release, NameWithVsn ++ ".boot"]),
                 filename:join(["releases", RelVsn, "start.boot"]),
                 []),
      add_to_tar(Tar,
                 filename:join([Release, NameWithVsn ++ ".rel"]),
                 filename:join(["releases", NameWithVsn ++ ".rel"]),
                 []),

      % Close
      erl_tar:close(Tar);
    {error, Reason} ->
      ?HALT("Can't create release archive; ~p", [Reason])
  end,

  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State.

add_to_tar(Tar, File, NameInArchive, Opts) ->
  ?DEBUG("= Add ~s with name ~s in tar", [File, NameInArchive]),
  case erl_tar:add(Tar, File, NameInArchive, Opts) of
    {error, {F, R}} ->
      ?HALT("!!! Can't add ~s in ~s", [F, R]);
    _ ->
      ok
  end.

get_up_from(ReleasesPaths, Name, Vsn) ->
  get_up_from(ReleasesPaths, Name, Vsn, []).
get_up_from([], _, _, Acc) ->
  Acc;
get_up_from([Path|Rest], Name, Vsn, Acc) ->
  case filename:basename(Path) of
    Vsn ->
      get_up_from(Rest, Name, Vsn, Acc);
    CurVsn ->
      RelFile = filename:join([Path, io_lib:format("~s-~s.rel", [Name, CurVsn])]),
      case filelib:is_file(RelFile) of
        true ->
          get_up_from(Rest, Name, Vsn, [strip_rel(RelFile)|Acc]);
        false ->
          ?HALT("Missing release for ~s version ~s", [Name, CurVsn])
      end
  end.

get_releases_path(Outdir, From, To) ->
  [Path || Path <- filelib:wildcard(
                     filename:join(
                       [Outdir, "releases", "*"])),
           vsn:compare(filename:basename(Path), From) > -1,
           vsn:compare(filename:basename(Path), To) < 1].

get_all_paths(ReleasesPaths, Name, Outdir) ->
  get_all_paths(ReleasesPaths, Name, Outdir, ReleasesPaths).
get_all_paths([], _, _, Acc) ->
  Acc;
get_all_paths([Path|Rest], Name, Outdir, Acc) ->
  Vsn = filename:basename(Path),
  RelFile = filename:join([Path, io_lib:format("~s-~s.rel", [Name, Vsn])]),
  Ebins = case file:consult(RelFile) of
            {ok, [{release, _, _, Deps}]} ->
              lists:foldl(fun({N, V}, Acc1) ->
                              P = bucs:to_string(
                                    filename:join([Outdir, 
                                                   "lib", 
                                                   io_lib:format("~s-~s", [N, V]), 
                                                   "ebin"])),
                              case lists:member(P, Acc1) of
                                true ->
                                  Acc1;
                                false ->
                                  [P|Acc1]
                              end
                          end, Acc, Deps);
            _ ->
              ?HALT("Missing rel file for ~s (version ~s)", [Name, Vsn])
          end,
  get_all_paths(Rest, Name, Outdir, Ebins).

strip_rel(Name) ->
  bucs:to_string(
    filename:join(
      filename:dirname(Name),
      filename:basename(Name, ".rel"))).

