% @hidden
-module(jorel_provider_dockerize).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).

-define(PROVIDER, dockerize).
-define(BUILD(Name), [
                      {add, [".", "/tmp/" ++ Name]},
                      {workdir, "/tmp/" ++ Name},
                      {cmd, "make jorel.release"}
                     ]).

init(State) ->
  jorel_config:add_provider(
    State,
    {?PROVIDER,
     #{
        module => ?MODULE,
        depends => [], % TODO: [release],
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
  {Build, State1} = case lists:keyfind(build, 1, Data) of
            {build, BuildConf} ->
              build_in_docker(State, BuildConf, From, Maintainer);
            _ ->
              build(State)
          end,

  ?INFO("* Dockerize ~s", [Build]),
  ?DEBUG("* TODO", []),

  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State1.

build(State) ->
  State1 = jorel_provider:run(State, release),
  {outdir, Build} = jorel_config:get(State1, outdir),
  {Build, State1}.

build_in_docker(State, Conf, From, Maintainer) ->
  Dockerfile = tempfile:name("Dockerfile.build.", [{ext, ""}]),
  {relname, RelName} = jorel_config:get(State, relname),
  case file:open(Dockerfile, [write,binary]) of
    {ok, FD} ->
      ?INFO("* Create ~s", [Dockerfile]),
      DockerfileData = [From, Maintainer] ++
        buclists:keyfind(prebuild, 1, Conf, []) ++
        ?BUILD(bucs:to_string(RelName)) ++
        buclists:keyfind(postbuild, 1, Conf, []),
      ?DEBUG("~p", [DockerfileData]),
      _ = dockerfile(FD, DockerfileData),
      _ = file:close(FD);
    {error, Reason} ->
      ?HALT("!!! Can't create ~s: ~p", [Dockerfile, Reason])
  end,
  ?DEBUG("* docker build --file=~s -t erlang_dev .", [Dockerfile]),
  ?DEBUG("* docker run --name tmp_build erlang_dev", []),
  ?DEBUG("* docker cp tmp_build:/tmp/pipo/jorel.tar .", []),
  ?DEBUG("* docker rm tmp_build", []),
  ?DEBUG("* docker rmi erlang_dev", []),
  Build = "TODO",
  {Build, State}.

% dockerize(State, Data) ->
%   Dockerfile = tempfile:name("Dockerfile.", [{ext, ""}]),
%   case file:open(Dockerfile, [write,binary]) of
%     {ok, FD} ->
%       ?INFO("* Create ~s", [Dockerfile]),
%       _ = create_dockerfile(FD, State, Data),
%       _ = file:close(FD);
%     {error, Reason} ->
%       ?HALT("!!! Can't create ~s: ~p", [Dockerfile, Reason])
%   end,
%   ?INFO("* Create container", []),
%   Password = string:strip(io:get_line("Password: "), both, $\n),
%   % ?DEBUG("* >> ~s <<", [Password]),
%   ?DEBUG("* echo \"~s\" | sudo -S docker build --file=~s .", [Password, Dockerfile]),
% 
%   % TODO:
%   ?INFO("* Delete ~s", [Dockerfile]),
%   _ = file:delete(Dockerfile),
%   State.

dockerfile(_, []) -> ok;
dockerfile(FD, [{label, Key, Value}|Data]) ->
  file:write(FD, io_lib:format("LABEL ~s=\"~s\"~n", [Key, Value])),
  dockerfile(FD, Data);
dockerfile(FD, [{env, Key, Value}|Data]) ->
  file:write(FD, io_lib:format("ENV ~s=~s~n", [Key, Value])),
  dockerfile(FD, Data);
dockerfile(FD, [{expose, Ports}|Data]) ->
  file:write(FD, io_lib:format("EXPOSE ~s~n", [string:join([bucs:to_string(E) || E <- Ports], " ")])),
  dockerfile(FD, Data);
dockerfile(FD, [{Cmd, Paths}|Data]) when Cmd == add; Cmd == copy ->
  file:write(FD, io_lib:format("~s [~s]~n", 
                               [string:to_upper(bucs:to_string(Cmd)),
                                string:join(["\"" ++ bucs:to_string(P) ++ "\"" || P <- Paths], ", ")])),
  dockerfile(FD, Data);
dockerfile(FD, [{Cmd, Param}|Data]) ->
  file:write(FD, io_lib:format("~s ~s~n", [string:to_upper(bucs:to_string(Cmd)), Param])),
  dockerfile(FD, Data).

