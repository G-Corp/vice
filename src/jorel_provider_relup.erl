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
  Rel = strip_rel(CurrentRel),

  ReleasesPaths = get_releases_path(Outdir, UpFrom, RelVsn),
  UpDown = get_up_from(ReleasesPaths, RelName, RelVsn),
  case UpDown of
    [] ->
      ?WARN("!!! No previous ~s release found", [RelName]);
    _ ->
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
      end
  end,

  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State.

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

