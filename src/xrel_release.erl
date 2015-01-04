-module(xrel_release).
-include("../include/xrel.hrl").

-export([
         make_root/1,
         resolv_apps/1,
         make_lib/2,
         make_release/2,
         make_bin/1,
         include_erts/1
        ]).

make_root(State) ->
  {outdir, Outdir} = xrel_config:get(State, outdir),
  case efile:make_dir(Outdir) of
    ok ->
      ?INFO("* Create directory ~s", [Outdir]);
    {error, Reason} ->
      ?HALT("Failed to create ~s: ~p", [Outdir, Reason])
  end.

resolv_apps(State) ->
  {release, {_, _}, Apps} = xrel_config:get(State, release),
  resolv_apps(State, Apps, []).

make_lib(State, Apps) ->
  {outdir, Outdir} = xrel_config:get(State, outdir),
  LibDir = filename:join(Outdir, "lib"),
  Src = case xrel_config:get(State, include_src, false) of
          {include_src, true} -> ["src", "include"];
          {include_src, false} -> []
        end,
  case efile:make_dir(LibDir) of
    ok ->
      lists:foreach(fun(#{app := App, vsn := Vsn, path := Path}) ->
                        copy_deps(App, Vsn, Path, LibDir, Src)
                    end, Apps);
    {error, Reason} ->
      ?HALT("Failed to create ~s: ~p", [LibDir, Reason])
  end.

make_release(State, Apps) ->
  {outdir, Outdir} = xrel_config:get(State, outdir),
  {relvsn, Vsn} = xrel_config:get(State, relvsn),
  RelDir = filename:join([Outdir, "releases", Vsn]),
  ?INFO("* Create ~s", [RelDir]),
  case efile:make_dir(RelDir) of 
    ok ->
      make_rel_file(State, RelDir, "vm.args", vm_args),
      make_rel_file(State, RelDir, "sys.config", sys_config),
      make_release_file(State, RelDir, Apps);
    {error, Reason} ->
      ?HALT("Failed to create ~s: ~p", [RelDir, Reason])
  end.

make_bin(State) ->
  {binfile, BinFile} = xrel_config:get(State, binfile),
  ?INFO("* Generate ~s", [BinFile]),
  _ = case efile:make_dir(filename:dirname(BinFile)) of
    ok -> ok;
    {error, Reason} ->
      ?HALT("Failed to create ~s: ~p", [BinFile, Reason])
  end,
  case run_dtl:render([]) of
    {ok, Data} ->
      case file:write_file(BinFile, Data) of
        ok -> 
          case file:change_mode(BinFile, 8#777) of
            ok -> ok;
            {error, Reason1} ->
              ?HALT("Error while creating ~s: ~p", [BinFile, Reason1])
          end;
        {error, Reason1} ->
          ?HALT("Error while creating ~s: ~p", [BinFile, Reason1])
      end;
    {error, Reason1} ->
      ?HALT("Error while creating ~s: ~p", [BinFile, Reason1])
  end.

include_erts(State) ->
  case case xrel_config:get(State, include_erts, true) of
         {include_erts, false} -> false;
         {include_erts, true} -> code:root_dir();
         {include_erts, X} when is_list(X) -> filename:absname(X);
         {include_erts, Y} ->
           ?HALT("Invalid value for parameter include_erts: ~p", [Y])
       end of
    false ->
      ok;
    Path ->
      ?INFO("* Add ets ~s from ~s", [erlang:system_info(version), Path]),
      {outdir, Outdir} = xrel_config:get(State, outdir),
      efile:copy(
        filename:join(Path, "erts-" ++ erlang:system_info(version)), 
        Outdir,
        [recursive])
  end.

% Private

resolv_apps(_, [], Apps) -> Apps;
resolv_apps(State, [App|Rest], Apps) ->
  {App, Vsn, Path, Deps} = case resolv_app(State, filename:join("**", "ebin"), App) of
                             notfound -> 
                               case resolv_app(State, filename:join([code:root_dir(), "lib", "**"]), App) of
                                 notfound ->
                                   ?HALT("Can't find application ~s", [App]);
                                 R -> R
                               end;
                             R -> R
                           end,
  resolv_apps(
    State,
    lists:umerge(lists:sort(Rest), lists:sort(Deps)),
    [#{app => App, vsn => Vsn, path => Path}| Apps]).

resolv_app(State, Path, Name) ->
  {exclude_path, Exclude} = xrel_config:get(State, exclude_path),
  case efile:wildcard(
         filename:join(Path, eutils:to_list(Name) ++ ".app"),
         Exclude
        ) of
    [] -> notfound;
    [AppFile|_] -> 
      AppPathFile = efile:expand_path(AppFile),
      case file:consult(AppPathFile) of
        {ok, [{application, Name, Config}]} ->
          Vsn = case lists:keyfind(vsn, 1, Config) of
                  {vsn, Vsn1} -> Vsn1;
                  _ -> "0"
                end,
          Deps = case lists:keyfind(applications, 1, Config) of
                   {applications, Apps} -> Apps;
                   _ -> []
                 end,
          {Name, Vsn, app_path(Name, Vsn, AppPathFile), Deps};
        _ -> 
          ?HALT("Invalid app file: ~s", [AppPathFile])
      end
  end.

app_path(App, Vsn, Path) ->
  Dirname = filename:dirname(Path),
  AppName = eutils:to_list(App) ++ "-" ++ Vsn,
  case string:str(Dirname, AppName) of
    0 ->
      case string:str(Dirname, eutils:to_list(App)) of
        0 ->
          ?HALT("Can't find root path for ~s", [App]);
        N ->
          string:substr(Dirname, 1, N + length(eutils:to_list(App)))
      end;
    N -> 
      string:substr(Dirname, 1, N + length(AppName))
  end.

copy_deps(App, Vsn, Path, Dest, Extra) ->
  ?INFO("* Copy ~s version ~s", [App, Vsn]),
  efile:copy(Path, Dest, [recursive, {only, ["ebin", "priv"] ++ Extra}]),
  FinalDest = filename:join(Dest, eutils:to_list(App) ++ "-" ++ Vsn),
  CopyDest = filename:join(Dest, filename:basename(Path)),
  if
    FinalDest =:= CopyDest -> ok;
    true ->
      _ = case filelib:is_dir(FinalDest) of
            true ->
              case efile:remove_recursive(FinalDest) of
                ok -> ok;
                {error, Reason} ->
                  ?HALT("Can't remove ~s: ~p", [FinalDest, Reason])
              end;
            false ->
              ok
          end,
      case file:rename(CopyDest, FinalDest) of
        ok -> 
          ?INFO("* Move ~s to ~s", [CopyDest, FinalDest]);
        {error, Reason1} ->
          ?HALT("Can't rename ~s: ~p", [CopyDest, Reason1])
      end
  end.

make_rel_file(State, RelDir, File, Type) ->
  Dest = filename:join(RelDir, File),
  ?INFO("* Create ~s", [Dest]),
  case xrel_config:get(State, Type, false) of
    {Type, false} ->
      Mod = eutils:to_atom(eutils:to_list(Type) ++ "_dtl"),
      {relname, RelName} = xrel_config:get(State, relname),
      case Mod:render([{relname, RelName}]) of
        {ok, Data} ->
          case file:write_file(Dest, Data) of
            ok -> ok;
            {error, Reason1} ->
              ?HALT("Error while creating ~s: ~p", [Dest, Reason1])
          end;
        {error, Reason} ->
          ?HALT("Error while creating ~s: ~p", [Dest, Reason])
      end;
    {Type, Src} ->
      case file:copy(Src, Dest) of
        {ok, _} -> ok;
        {error, Reason} ->
          ?HALT("Can't copy ~s to ~s: ~p", [Src, Dest, Reason])
      end
  end.

make_release_file(State, RelDir, Apps) ->
  {relname, Name} = xrel_config:get(State, relname),
  {relvsn, Vsn} = xrel_config:get(State, relvsn),
  Params = [
            {relname, Name},
            {relvsn, Vsn},
            {ertsvsn, erlang:system_info(version)},
            {apps, lists:map(fun(#{app := App, vsn := AppVsn}) ->
                                 {App, AppVsn}
                             end, Apps)}
           ],
  Dest = filename:join(RelDir, eutils:to_list(Name) ++ ".rel"),
  ?INFO("* Create ~s", [Dest]),
  case rel_dtl:render(Params) of
    {ok, Data} ->
      case file:write_file(Dest, Data) of
        ok -> ok;
        {error, Reason1} ->
          ?HALT("Error while creating ~s: ~p", [Dest, Reason1])
      end;
    {error, Reason} ->
      ?HALT("Error while creating ~s: ~p", [Dest, Reason])
  end.

