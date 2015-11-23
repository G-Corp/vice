-module(jorel_release).
-include("../include/jorel.hrl").

-export([
         make_root/1,
         resolv_apps/1,
         resolv_boot/2,
         make_lib/2,
         make_release/3,
         make_bin/1,
         include_erts/1,
         make_boot_script/2,
         make_relup/2
        ]).

make_root(State) ->
  {outdir, Outdir} = jorel_config:get(State, outdir),
  case efile:make_dir(Outdir) of
    ok ->
      ?INFO("* Create directory ~s", [Outdir]);
    {error, Reason} ->
      ?HALT("!!! Failed to create ~s: ~p", [Outdir, Reason])
  end.

resolv_apps(State) ->
  {release, {_, _}, Apps} = jorel_config:get(State, release),
  Apps1 = case jorel_config:get(State, all_deps, false) of
    {all_deps, true} -> find_all_deps(State, Apps);
    _ -> Apps
  end,
  resolv_apps(State, Apps1, [], []).

resolv_boot(State, AllApps) ->
  case jorel_config:get(State, boot, all) of
    {boot, all} -> AllApps;
    {boot, Apps} -> resolv_apps(State, Apps, [], [])
  end.

make_lib(State, Apps) ->
  {outdir, Outdir} = jorel_config:get(State, outdir),
  LibDir = filename:join(Outdir, "lib"),
  Src = case jorel_config:get(State, include_src, false) of
          {include_src, true} -> ["src", "include"];
          {include_src, false} -> []
        end,
  case efile:make_dir(LibDir) of
    ok ->
      lists:foreach(fun(#{app := App, vsn := Vsn, path := Path}) ->
                        copy_deps(App, Vsn, Path, LibDir, Src)
                    end, Apps);
    {error, Reason} ->
      ?HALT("!!! Failed to create ~s: ~p", [LibDir, Reason])
  end.

make_release(State, AllApps, BootApps) ->
  {outdir, Outdir} = jorel_config:get(State, outdir),
  {relvsn, Vsn} = jorel_config:get(State, relvsn),
  RelDir = filename:join([Outdir, "releases", Vsn]),
  ?INFO("* Create ~s", [RelDir]),
  case efile:make_dir(RelDir) of
    ok ->
      _ = make_rel_file(State, RelDir, "vm.args", vm_args),
      _ = case jorel_elixir:exist() of
            true ->
              Dest = filename:join(RelDir, "sys.config"),
              case jorel_config:get(State, sys_config, false) of
                {sys_config, false} ->
                  {relname, RelName} = jorel_config:get(State, relname),
                  case sys_config_dtl:render([{relname, RelName}]) of
                    {ok, Data} ->
                      case file:write_file(Dest, Data) of
                        ok -> Dest;
                        {error, Reason1} ->
                          ?HALT("!!! Error while creating ~s: ~p", [Dest, Reason1])
                      end;
                    {error, Reason} ->
                      ?HALT("!!! Error while creating ~s: ~p", [Dest, Reason])
                  end;
                {sys_config, Src} ->
                  {mix_env, MixEnv} = jorel_config:get(State, mix_env, prod),
                  ?INFO("* Create ~s from ~s (env ~s)", [Dest, Src, MixEnv]),
                  case jorel_elixir:config_to_sys_config(Src, Dest, MixEnv) of
                    ok -> 
                      ok;
                    error ->
                      ?HALT("!!! Can't create ~s from ~s", [Dest, Src])
                  end
              end;
            false ->
              make_rel_file(State, RelDir, "sys.config", sys_config)
          end,
      jorel_tempdir:mktmp(fun(TmpDir) ->
                             BootErl = make_rel_file(State, TmpDir, "extrel.erl", extrel),
                             BootExe = jorel_escript:build(BootErl, filename:dirname(BootErl)),
                             case efile:copyfile(BootExe, filename:join(RelDir, filename:basename(BootExe))) of
                               ok -> ok;
                               {error, Reason2} ->
                                 ?HALT("Can't copy ~s: ~p", [BootExe, Reason2])
                             end
                         end),
      _ = make_release_file(State, RelDir, AllApps, "-" ++ Vsn ++ ".deps"),
      RelFile = make_release_file(State, RelDir, BootApps, "-" ++ Vsn ++ ".rel"),
      ?INFO("* Create RELEASES file", []),
      case release_handler:create_RELEASES(Outdir, RelDir, RelFile, []) of
        ok -> ok;
        {error, Reason3} ->
          ?HALT("!!! Failed to create RELEASES: ~p", [Reason3])
      end;
    {error, Reason} ->
      ?HALT("!!! Failed to create ~s: ~p", [RelDir, Reason])
  end.

make_boot_script(State, BootApps) ->
  {outdir, Outdir} = jorel_config:get(State, outdir),
  {relname, Relname} = jorel_config:get(State, relname),
  {relvsn, RelVsn} = jorel_config:get(State, relvsn),
  AppsPaths = lists:foldl(
                fun(#{app := App, vsn := Vsn}, Acc) ->
                    filelib:wildcard(
                      filename:join(
                        [Outdir, "lib", eutils:to_list(App) ++ "-" ++ Vsn, "**", "ebin"]
                       )) ++ Acc
                end, [], BootApps),
  RelDir = filename:join([Outdir, "releases", RelVsn]),
  Paths = [RelDir|AppsPaths],
  ?INFO("* Create boot script", []),
  case systools:make_script(
         eutils:to_list(Relname) ++ "-" ++ RelVsn,
         [{path, Paths},
          {outdir, RelDir},
          silent]) of
    error ->
      ?HALT("!!! Can't generate boot script", []);
    {error, _, Error} ->
      ?HALT("!!! Error while generating boot script : ~p", [Error]);
    {ok, _, []} ->
      ok;
    {ok, _, Warnings} ->
      ?DEBUG("! Generate boot script : ~p", [Warnings]);
    _ ->
      ok
  end.

make_bin(State) ->
  {binfile, BinFile} = jorel_config:get(State, binfile),
  {relvsn, Vsn} = jorel_config:get(State, relvsn),
  {relname, Name} = jorel_config:get(State, relname),
  BinFileWithVsn = BinFile ++ "-" ++ Vsn,
  ?INFO("* Generate ~s", [BinFile]),
  _ = case efile:make_dir(filename:dirname(BinFile)) of
    ok -> ok;
    {error, Reason} ->
      ?HALT("!!! Failed to create ~s: ~p", [BinFile, Reason])
  end,
  case run_dtl:render([{relvsn, Vsn}, {relname, Name}, {ertsvsn, erlang:system_info(version)}]) of
    {ok, Data} ->
      case file:write_file(BinFile, Data) of
        ok ->
          ?INFO("* Generate ~s", [BinFileWithVsn]),
          Bins = case file:copy(BinFile, BinFileWithVsn) of
            {ok, _} ->
              [BinFile, BinFileWithVsn];
            {error, Reason2} ->
              ?ERROR("Error while creating ~s: ~p", [BinFileWithVsn, Reason2]),
              [BinFile]
          end,
          lists:foreach(fun(Bin) ->
                            case file:change_mode(Bin, 8#777) of
                              ok -> ok;
                              {error, Reason1} ->
                                ?HALT("!!! Can't set executable to ~s: ~p", [Bin, Reason1])
                            end
                        end, Bins);
        {error, Reason1} ->
          ?HALT("!!! Error while creating ~s: ~p", [BinFile, Reason1])
      end;
    {error, Reason1} ->
      ?HALT("!!! Error while creating ~s: ~p", [BinFile, Reason1])
  end.

include_erts(State) ->
  case case jorel_config:get(State, include_erts, true) of
         {include_erts, false} -> false;
         {include_erts, true} -> code:root_dir();
         {include_erts, X} when is_list(X) -> filename:absname(X);
         {include_erts, Y} ->
           ?HALT("!!! Invalid value for parameter include_erts: ~p", [Y])
       end of
    false ->
      ok;
    Path ->
      ?INFO("* Add ets ~s from ~s", [erlang:system_info(version), Path]),
      {outdir, Outdir} = jorel_config:get(State, outdir),
      efile:copy(
        filename:join(Path, "erts-" ++ erlang:system_info(version)),
        Outdir,
        [recursive]),
      ErtsBinDir = filename:join([Outdir, "erts-" ++ erlang:system_info(version), "bin"]),
      ?INFO("* Substituting in erl.src and start.src to form erl and start", []),
      subst_src_scripts(["erl", "start"], ErtsBinDir, ErtsBinDir,
                        [{"FINAL_ROOTDIR", "`cd $(dirname $0)/../../ && pwd`"},
                         {"EMU", "beam"}],
                        [preserve]),
      %%! Workaround for pre OTP 17.0: start.src does
      %%! not have correct permissions, so the above 'preserve' option did not help
      ok = file:change_mode(filename:join(ErtsBinDir, "start"), 8#0755),
      ?INFO("* Install start_clean.boot", []),
      Prefix = code:root_dir(),
      ok = efile:copy(filename:join([Prefix, "bin", "start_clean.boot"]),
                      filename:join([Outdir, "bin", "start_clean.boot"])),
      ?INFO("* Install start.boot", []),
      Prefix = code:root_dir(),
      ok = efile:copy(filename:join([Prefix, "bin", "start.boot"]),
                      filename:join([Outdir, "bin", "start.boot"]))
  end.

make_relup(State, AllApps) ->
  case jorel_config:get(State, disable_relup, true) of
    {disable_relup, true} ->
      ?DEBUG("* relup disabled", []);
    _ ->
      case lists:any(fun(#{app := sasl}) -> true;
                    (_) -> false
                 end, AllApps) of
        true ->
          {relname, RelName} = jorel_config:get(State, relname),
          {relvsn, RelVsn} = jorel_config:get(State, relvsn),
          {outdir, Outdir} = jorel_config:get(State, outdir),
          VsnApp = eutils:to_string(RelName) ++ "-" ++ RelVsn,
          AppDir = filename:join([Outdir, "lib", VsnApp, "ebin", "*.appup"]),
          case filelib:wildcard(AppDir) of
            [AppupFile] ->
              case file:consult(AppupFile) of
                {ok, [{Vsn, UpFrom, DownTo}]} ->
                  ?INFO("* Use appup ~s", [AppupFile]),
                  {UpFrom2, Versions1} = get_apps_and_versions(RelName, UpFrom),
                  {DownTo2, Versions2} = get_apps_and_versions(RelName, DownTo),
                  Versions3 = lists:umerge(lists:sort([Vsn|Versions1]), lists:sort(Versions2)),
                  case lists:foldl(fun(CurrentApp, {Missing, Exist}) ->
                                       Path = filename:join([Outdir, "lib", CurrentApp, "ebin"]),
                                       case filelib:is_dir(Path) of
                                         true ->
                                           {Missing, [Path|Exist]};
                                         false ->
                                           {[CurrentApp|Missing], Exist}
                                       end
                                   end, {[], []},
                                   [VsnApp,
                                    lists:umerge(lists:sort(UpFrom2),
                                                 lists:sort(DownTo2))]) of
                    {[], AppsPath} ->
                      AppsPath2 = lists:foldl(fun(Version, AppsPath3) ->
                                                  [filename:join([Outdir, "releases", Version])|AppsPath3]
                                              end, AppsPath, Versions3),
                      ?INFO("* Generate relup file", []),
                      case systools:make_relup(
                             VsnApp,
                             UpFrom2,
                             DownTo2,
                             [{path, AppsPath2},
                              {outdir, filename:join([Outdir, "releases", RelVsn])},
                              warnings_as_errors]) of
                        {error, _, Error} ->
                          ?HALT("!!! Create relup faild: ~p", [Error]);
                        error ->
                          ?HALT("!!! Create relup faild", []);
                        _ ->
                          RelArchive = filename:join([Outdir, "releases", VsnApp ++ ".tar.gz"]),
                          case erl_tar:open(RelArchive, [write, compressed]) of
                            {ok, TD} ->
                              ?INFO("* Create release archive: ~s", [RelArchive]),
                              lists:foreach(
                                fun(#{app := AppName,
                                      vsn := AppVsn}) ->
                                    LN = eutils:to_string(AppName) ++ "-" ++ AppVsn,
                                    IF = filename:join([Outdir, "lib", LN]),
                                    OF = filename:join(["lib", LN]),
                                    lists:foreach(fun(F) ->
                                                      case erl_tar:add(TD, F, filename:join([OF, efile:relative_from(F, IF)]), []) of
                                                        {error, {F, R}} -> ?HALT("!!! Can't add ~s in release archive: ~p", [F, R]);
                                                        _ -> ok
                                                      end
                                                  end, filelib:wildcard(filename:join([IF, "**", "*"])))
                                end, AllApps),
                              RI = filename:join([Outdir, "releases", RelVsn]),
                              lists:foreach(fun(F) ->
                                                case erl_tar:add(TD, F, efile:relative_from(F, Outdir), []) of
                                                  {error, {F, R}} -> ?HALT("!!! Can't add ~s in release archive: ~p", [F, R]);
                                                  _ -> ok
                                                end
                                            end, filelib:wildcard(filename:join([RI, "**", "*"]))),
                              case erl_tar:add(TD, filename:join([RI, VsnApp ++ ".rel"]), filename:join(["releases", VsnApp ++ ".rel"]), []) of
                                {error, {F, R}} -> ?HALT("!!! Can't add ~s in release archive: ~p", [F, R]);
                                _ -> ok
                              end,
                              case erl_tar:add(TD, filename:join([RI, VsnApp ++ ".boot"]), filename:join(["releases", RelVsn, "start.boot"]), []) of
                                {error, {F3, R3}} -> ?HALT("!!! Can't add ~s in release archive: ~p", [F3, R3]);
                                _ -> ok
                              end,
                              BI = filename:join([Outdir, "bin"]),
                              case erl_tar:add(TD, filename:join([BI, VsnApp]), filename:join(["bin", VsnApp]), []) of
                                {error, {F1, R1}} -> ?HALT("!!! Can't add ~s in release archive: ~p", [F1, R1]);
                                _ -> ok
                              end,
                              case erl_tar:add(TD, filename:join([BI, RelName]), filename:join(["bin", RelName]), []) of
                                {error, {F2, R2}} -> ?HALT("!!! Can't add ~s in release archive: ~p", [F2, R2]);
                                _ -> ok
                              end,
                              erl_tar:close(TD);
                            {error, Reason} ->
                              ?DEBUG("Can't create release archive: ~p",
                                     [Reason])
                          end
                      end;
                    {Missing, _} ->
                      ?DEBUG("* Can't create relup, missing apps ~s", [string:join(Missing, ", ")])
                  end;
                _ ->
                  ?DEBUG("* Invalid appup file (~s)", [AppupFile])
              end;
            _ ->
              ?DEBUG("* No appup found", [])
          end;
        false ->
          ?DEBUG("* SASL not present, can't create relup file", [])
      end
  end.

% Private

get_apps_and_versions(App, From) ->
  lists:foldl(fun({Vsn, _}, {Apps, Versions}) ->
                  {[eutils:to_string(App) ++ "-" ++ Vsn|Apps],
                   [Vsn|Versions]}
              end, {[], []}, From).

find_all_deps(State, Apps) ->
  {exclude_dirs, Exclude} = jorel_config:get(State, exclude_dirs),
  case efile:wildcard(
         filename:join(["**", "ebin", "*.app"]),
         Exclude
        ) of
    [] -> Apps;
    DepsApps ->
      lists:foldl(fun(Path, Acc) ->
                      App = filename:basename(Path, ".app"),
                      case elists:include(Acc, App) of
                        true -> Acc;
                        false -> [eutils:to_atom(App)|Acc]
                      end
                  end, Apps, DepsApps)
  end.

resolv_apps(_, [], _, Apps) -> Apps;
resolv_apps(State, [App|Rest], Done, AllApps) ->
  {ignore_deps, IgnoreDeps} = jorel_config:get(State, ignore_deps, []),
  case lists:member(App, IgnoreDeps) of
    false ->
      {App, Vsn, Path, Deps} = case resolv_app(State, filename:join("**", "ebin"), App) of
                                 notfound ->
                                   case resolv_app(State, filename:join([code:root_dir(), "lib", "**"]), App) of
                                     notfound ->
                                       case jorel_elixir:exist() of
                                         true ->
                                           case jorel_elixir:path() of
                                             {ok, ElixirPath} ->
                                               case resolv_app(State, filename:join([ElixirPath, "**"]), App) of
                                                 notfound ->
                                                   ?HALT("!!! Can't find application ~s", [App]);
                                                 R -> R
                                               end;
                                             _ ->
                                               ?HALT("!!! Can't find application ~s", [App])
                                           end;
                                         false -> 
                                           ?HALT("!!! Can't find application ~s", [App])
                                       end;
                                     R -> R
                                   end;
                                 R -> R
                               end,
      Done1 = [App|Done],
      Rest1 = elists:delete_if(fun(A) ->
                                   elists:include(Done1, A)
                               end, lists:umerge(lists:sort(Rest), lists:sort(Deps))),
      resolv_apps(
        State,
        Rest1,
        Done1,
        [#{app => App, vsn => Vsn, path => Path}| AllApps]);
    true ->
      resolv_apps(State, Rest, Done, AllApps)
  end.

resolv_app(State, Path, Name) ->
  {exclude_dirs, Exclude} = jorel_config:get(State, exclude_dirs),
  case efile:wildcard(
         filename:join(Path, eutils:to_list(Name) ++ ".app"),
         Exclude
        ) of
    [] -> notfound;
    [AppFile|_] ->
      ?INFO("= Found ~s", [AppFile]),
      AppPathFile = efile:expand_path(AppFile),
      case file:consult(AppPathFile) of
        {ok, [{application, Name, Config}]} ->
          Vsn = case lists:keyfind(vsn, 1, Config) of
                  {vsn, Vsn1} -> Vsn1;
                  _ -> "0"
                end,
          Deps = lists:foldl(fun(Type, Acc) ->
                                 case lists:keyfind(Type, 1, Config) of
                                   {Type, Apps} -> Acc ++ Apps;
                                   _ -> Acc
                                 end
                             end, [], [applications, included_applications]),
          {Name, Vsn, app_path(Name, Vsn, AppPathFile), Deps};
        E ->
          ?HALT("!!! Invalid ~p.app file ~s: ~p", [Name, AppPathFile, E])
      end
  end.

app_path(App, Vsn, Path) ->
  Dirname = filename:dirname(Path),
  AppName = eutils:to_list(App) ++ "-" ++ Vsn,
  case string:rstr(Dirname, AppName) of
    0 ->
      case string:rstr(Dirname, eutils:to_list(App)) of
        0 ->
          ?HALT("!!! Can't find root path for ~s", [App]);
        N ->
          string:substr(Dirname, 1, N + length(eutils:to_list(App)))
      end;
    N ->
      string:substr(Dirname, 1, N + length(AppName))
  end.

copy_deps(App, Vsn, Path, Dest, Extra) ->
  ?INFO("* Copy ~s version ~s (~s)", [App, Vsn, Path]),
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
                  ?HALT("!!! Can't remove ~s: ~p", [FinalDest, Reason])
              end;
            false ->
              ok
          end,
      case file:rename(CopyDest, FinalDest) of
        ok ->
          ?INFO("* Move ~s to ~s", [CopyDest, FinalDest]);
        {error, Reason1} ->
          ?HALT("!!! Can't rename ~s: ~p", [CopyDest, Reason1])
      end
  end.

make_rel_file(State, RelDir, File, Type) ->
  Dest = filename:join(RelDir, File),
  ?INFO("* Create ~s", [Dest]),
  case jorel_config:get(State, Type, false) of
    {Type, false} ->
      Mod = eutils:to_atom(eutils:to_list(Type) ++ "_dtl"),
      {relname, RelName} = jorel_config:get(State, relname),
      case Mod:render([{relname, RelName}]) of
        {ok, Data} ->
          case file:write_file(Dest, Data) of
            ok -> Dest;
            {error, Reason1} ->
              ?HALT("!!! Error while creating ~s: ~p", [Dest, Reason1])
          end;
        {error, Reason} ->
          ?HALT("!!! Error while creating ~s: ~p", [Dest, Reason])
      end;
    {Type, Src} ->
      case file:copy(Src, Dest) of
        {ok, _} -> Dest;
        {error, Reason} ->
          ?HALT("!!! Can't copy ~s to ~s: ~p", [Src, Dest, Reason])
      end
  end.

make_release_file(State, RelDir, Apps, Ext) ->
  {relname, Name} = jorel_config:get(State, relname),
  {relvsn, Vsn} = jorel_config:get(State, relvsn),
  Params = [
            {relname, Name},
            {relvsn, Vsn},
            {ertsvsn, erlang:system_info(version)},
            {apps, lists:map(fun maps:to_list/1, Apps)}
           ],
  Dest = filename:join(RelDir, eutils:to_list(Name) ++ Ext),
  ?INFO("* Create ~s", [Dest]),
  case rel_dtl:render(Params) of
    {ok, Data} ->
      case file:write_file(Dest, Data) of
        ok -> Dest;
        {error, Reason1} ->
          ?HALT("!!! Error while creating ~s: ~p", [Dest, Reason1])
      end;
    {error, Reason} ->
      ?HALT("!!! Error while creating ~s: ~p", [Dest, Reason])
  end.

subst_src_scripts(Scripts, SrcDir, DestDir, Vars, Opts) ->
  lists:foreach(fun(Script) ->
                    subst_src_script(Script, SrcDir, DestDir,
                                     Vars, Opts)
                end, Scripts).

subst_src_script(Script, SrcDir, DestDir, Vars, Opts) ->
  subst_file(filename:join([SrcDir, Script ++ ".src"]),
             filename:join([DestDir, Script]),
             Vars, Opts).

subst_file(Src, Dest, Vars, Opts) ->
  {ok, Conts} = read_txt_file(Src),
  NConts = subst(Conts, Vars),
  write_file(Dest, NConts),
  case lists:member(preserve, Opts) of
    true ->
      {ok, FileInfo} = file:read_file_info(Src),
      file:write_file_info(Dest, FileInfo);
    false ->
      ok
  end.

read_txt_file(File) ->
  {ok, Bin} = file:read_file(File),
  {ok, binary_to_list(Bin)}.

write_file(FName, Conts) ->
  Enc = file:native_name_encoding(),
  {ok, Fd} = file:open(FName, [write]),
  file:write(Fd, unicode:characters_to_binary(Conts, Enc, Enc)),
  file:close(Fd).

subst(Str, Vars) ->
  subst(Str, Vars, []).

subst([$%, C| Rest], Vars, Result) when $A =< C, C =< $Z ->
  subst_var([C| Rest], Vars, Result, []);
subst([$%, C| Rest], Vars, Result) when $a =< C, C =< $z ->
  subst_var([C| Rest], Vars, Result, []);
subst([$%, C| Rest], Vars, Result) when  C == $_ ->
  subst_var([C| Rest], Vars, Result, []);
subst([C| Rest], Vars, Result) ->
  subst(Rest, Vars, [C| Result]);
subst([], _Vars, Result) ->
  lists:reverse(Result).

subst_var([$%| Rest], Vars, Result, VarAcc) ->
  Key = lists:reverse(VarAcc),
  case lists:keysearch(Key, 1, Vars) of
    {value, {Key, Value}} ->
      subst(Rest, Vars, lists:reverse(Value, Result));
    false ->
      subst(Rest, Vars, [$%| VarAcc ++ [$%| Result]])
  end;
subst_var([C| Rest], Vars, Result, VarAcc) ->
  subst_var(Rest, Vars, Result, [C| VarAcc]);
subst_var([], Vars, Result, VarAcc) ->
  subst([], Vars, [VarAcc ++ [$%| Result]]).

