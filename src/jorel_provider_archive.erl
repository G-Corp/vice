% @hidden
-module(jorel_provider_archive).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, archive).

init(State) ->
  jorel_config:add_provider(
    State,
    {?PROVIDER,
     #{
        module => ?MODULE,
        depends => [],
        desc => "Create a release archive"
       }
    }
   ).

do(State) ->
  ?INFO("== Start provider ~p", [?PROVIDER]),
  {relname, RelName} = jorel_config:get(State, relname),
  {relvsn, RelVsn} = jorel_config:get(State, relvsn),
  {outdir, Outdir} = jorel_config:get(State, outdir),
  NameWithVsn = io_lib:format("~s-~s", [RelName, RelVsn]),
  RelArchive = filename:join([Outdir, "releases", NameWithVsn ++ ".tar.gz"]),
  CurrentRel = filename:join([Outdir, "releases", RelVsn, NameWithVsn ++ ".rel"]),
  {ErtsVersion, Deps} = case file:consult(CurrentRel) of
                          {ok, [{release, _, {erts, V}, D}]} ->
                            {V, D};
                          _ ->
                            ?HALT("Missing release ~s (version ~s)", [RelName, RelVsn])
                        end,
  ?DEBUG("= ~s", [RelArchive]),
  case erl_tar:open(RelArchive, [write, compressed]) of
    {ok, Tar} ->
      % Add libs
      ?INFO("* Add libs", []),
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
          ?INFO("* Add Erts ~s", [ErtsVersion]),
          Erts = filename:join([Outdir, io_lib:format("erts-~s", [ErtsVersion])]),
          [add_to_tar(Tar, F, bucfile:relative_from(F, Outdir), [])
           || F <- filelib:wildcard(filename:join([Erts, "**", "*"]))]
      end,
      % Add bin
      ?INFO("* Add bin", []),
      Bin = filename:join([Outdir, "bin"]),
      [add_to_tar(Tar, F, bucfile:relative_from(F, Outdir), [])
       || F <- filelib:wildcard(filename:join([Bin, "**", "*"]))],
      % Add release
      Release = filename:join([Outdir, "releases", RelVsn]),
      ?INFO("* Add release files", []),
      [add_to_tar(Tar, F, bucfile:relative_from(F, Outdir), [])
       || F <- filelib:wildcard(filename:join([Release, "**", "*"]))],
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
  case filelib:is_dir(File) andalso (not (file:list_dir(File) == {ok, []})) of
    true ->
      ok;
    false ->
      ?DEBUG("* Add ~s with name ~s in tar", [File, NameInArchive]),
      case erl_tar:add(Tar, File, NameInArchive, Opts) of
        {error, {F, R}} ->
          ?HALT("!!! Can't add ~s in ~s", [F, R]);
        _ ->
          ok
      end
  end.

