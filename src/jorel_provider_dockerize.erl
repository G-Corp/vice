% @hidden
-module(jorel_provider_dockerize).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).

-define(PROVIDER, dockerize).
-define(BUILD(OutputDir, Name), [
                                 {copy, [".", "/app/" ++ Name ++ "/"]},
                                 {workdir, "/app/" ++ Name}
                                ]).
-define(BUILDCMD(OutputDir), [
                              {cmd, "rm -rf " ++ OutputDir ++ " && make distclean && make jorel.release"}
                             ]).

init(State) ->
  jorel_config:add_provider(
    State,
    {?PROVIDER,
     #{
        module => ?MODULE,
        depends => [],
        desc => "Create a Docker container with your App"
      }
    }
   ).

do(State) ->
  case jorel_config:get(State, dockerize) of
    {dockerize, []} ->
      ?HALT("!!! Missing dockerize configuration", []);
    {dockerize, Data} ->
      dockerize(State, Data)
  end.

dockerize(State, Data) ->
  ?INFO("== Start provider ~p", [?PROVIDER]),
  From = case lists:keyfind(from, 1, Data) of
           false ->
             ?HALT("!!! Missing from", []);
           From1 ->
             From1
         end,
  Maintainer = case lists:keyfind(maintainer, 1, Data) of
                 false ->
                   {maintainer, "Jorel"};
                 Maintainer1 ->
                   Maintainer1
               end,
  RemoveOrigin = case lists:keyfind(remove_origin, 1, Data) of
                   {remove_origin, true} ->
                     true;
                   _ ->
                     false
                 end,
  {Clean, Build, State1} = case lists:keyfind(build, 1, Data) of
            {build, BuildConf} ->
              build_in_docker(State, BuildConf, From, Maintainer, RemoveOrigin);
            _ ->
              build(State)
          end,

  ?INFO("* Dockerize ~s", [Build]),
  ?DEBUG("* TODO", []),

  _ = if 
    Clean ->
      ?DEBUG("* TODO Clean ~s", [Build]);
    true ->
      ok
  end,

  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State1.

build(State) ->
  State1 = jorel_provider:run(State, release),
  {outdir, Build} = jorel_config:get(State1, outdir),
  {false, Build, State1}.

build_in_docker(State, Conf, From, Maintainer, RemoveOrigin) ->
  Dockerfile = "Dockerfile.build",
  {from, FromImageName} = From,
  BuildImageName = string:to_lower("jbi_" ++ bucrandom:randstr(8)),
  BuildContainerName = string:to_lower("jbc_" ++ bucrandom:randstr(8)),
  {output_dir, OutputDir} = jorel_config:get(State, output_dir),
  BuildPath = string:to_lower(OutputDir ++ "_" ++ bucrandom:randstr(8)),
  {relname, RelName} = jorel_config:get(State, relname),
  {output_dir, OutputDir} = jorel_config:get(State, output_dir),
  DockerAppPath = filename:join(["/app", RelName, OutputDir]),
  case file:open(Dockerfile, [write,binary]) of
    {ok, FD} ->
      ?INFO("* Create ~s", [Dockerfile]),
      DockerfileData = [From, Maintainer] ++
        buclists:keyfind(prebuild, 1, Conf, []) ++
        ?BUILD(OutputDir, bucs:to_string(RelName)) ++
        buclists:keyfind(postbuild, 1, Conf, []) ++
        ?BUILDCMD(OutputDir),
      _ = dockerfile(FD, DockerfileData),
      _ = file:close(FD);
    {error, Reason} ->
      ?HALT("!!! Can't create ~s: ~p", [Dockerfile, Reason])
  end,
  ?INFO("* Prepare the build container (This can take a while... Go get yourself a cup of coffee.)", []),
  ?DEBUG("docker build --file=~s -q -t ~s .", [Dockerfile, BuildImageName]),
  _ = case sh:sh("docker build --file=~s -q -t ~s .", [Dockerfile, BuildImageName], [return_on_error]) of
        {ok, _} ->
          ok;
        {error, _} ->
          ?HALT("!!! Build image faild", [])
      end,
  ?INFO("* Create the release", []),
  _ = case sh:sh("docker run --name ~s ~s", [BuildContainerName, BuildImageName], [return_on_error]) of
        {ok, _} ->
          ok;
        {error, _} ->
          ?HALT("!!! Build release faild", [])
      end,
  ?INFO("* Copy ~s:~s to ~s", [BuildContainerName, DockerAppPath, BuildPath]),
  _ = case sh:sh("docker cp ~s:~s ~s", [BuildContainerName, DockerAppPath, BuildPath], [return_on_error]) of
        {ok, _} ->
          ok;
        {error, _} ->
          ?HALT("!!! Build release faild", [])
      end,
  ?INFO("* Remove container and image", []),
  _ = case sh:sh("docker rm ~s", [BuildContainerName], [return_on_error]) of
        {ok, _} ->
          ok;
        {error, _} ->
          ?ERROR("! Faild to remove container ~s", [BuildContainerName])
      end,
  _ = case sh:sh("docker rmi ~s", [BuildImageName], [return_on_error]) of
        {ok, _} ->
          ok;
        {error, _} ->
          ?ERROR("! Faild to remove image ~s", [BuildImageName])
      end,
  if
    RemoveOrigin ->
      _ = case sh:sh("docker rmi ~s", [FromImageName], [return_on_error]) of
            {ok, _} ->
              ok;
            {error, _} ->
              ?ERROR("! Faild to remove image ~s", [FromImageName])
          end;
    true ->
      ok
  end,
  {true, BuildPath, State}.

dockerfile(_, []) -> ok;
dockerfile(FD, [{label, Key, Value}|Data]) ->
  file:write(FD, io_lib:format("LABEL ~s=\"~s\"~n", [Key, Value])),
  dockerfile(FD, Data);
dockerfile(FD, [{env, Key, Value}|Data]) ->
  file:write(FD, io_lib:format("ENV ~s=\"~s\"~n", [Key, Value])),
  dockerfile(FD, Data);
dockerfile(FD, [{expose, Ports}|Data]) ->
  file:write(FD, io_lib:format("EXPOSE ~s~n", [string:join([bucs:to_string(E) || E <- Ports], " ")])),
  dockerfile(FD, Data);
dockerfile(FD, [{Cmd, Paths}|Data]) when Cmd == add; Cmd == copy; Cmd == entrypoint ->
  file:write(FD, io_lib:format("~s [~s]~n", 
                               [string:to_upper(bucs:to_string(Cmd)),
                                string:join(["\"" ++ bucs:to_string(P) ++ "\"" || P <- Paths], ", ")])),
  dockerfile(FD, Data);
dockerfile(FD, [{Cmd, Param}|Data]) ->
  file:write(FD, io_lib:format("~s ~s~n", [string:to_upper(bucs:to_string(Cmd)), Param])),
  dockerfile(FD, Data).

