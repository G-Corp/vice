% @hidden
-module(jorel_provider_dockerize).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).

-define(PROVIDER, dockerize).
-define(PREINSTALL(JorelVersion), [
                                   {run, "mkdir -p /opt"},
                                   {label, jorel, JorelVersion}
                                  ]).
-define(INSTALL(Output, AppName), [
                           {workdir, "/opt"},
                           {add, [filename:join([Output, AppName]), "/opt"]}
                          ]).
-define(POSTINSTALL(AppName), [
                               {workdir, "/opt/" ++ AppName},
                               {cmd, "./bin/" ++ AppName ++ " start"}
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
  ?INFO("== Start provider ~p", [?PROVIDER]),
  State1 = case jorel_config:get(State, dockerize, []) of
             {dockerize, []} ->
               ?HALT("!!! Missing dockerize configuration", []);
             {dockerize, Data} ->
               dockerize(State, Data)
           end,
  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State1.

dockerize(State, Data) ->
  Dockerfile = tempfile:name("Dockerfile.", [{ext, ""}]),
  case file:open(Dockerfile, [write,binary]) of
    {ok, FD} ->
      ?INFO("* Create ~s", [Dockerfile]),
      _ = create_dockerfile(FD, State, Data),
      _ = file:close(FD);
    {error, Reason} ->
      ?HALT("!!! Can't create ~s: ~p", [Dockerfile, Reason])
  end,
  ?INFO("* Create container", []),
  _Password = string:strip(io:get_line("Password: "), both, $\n),
  % ?DEBUG("* >> ~s <<", [Password]),
  ?DEBUG("* sudo docker build --file=~s .", [Dockerfile]),

  % TODO:
  ?INFO("* Delete ~s", [Dockerfile]),
  _ = file:delete(Dockerfile),
  State.

create_dockerfile(FD, State, Data) ->
  create_dockerfile1(FD, State, Data),
  {relname, Relname} = jorel_config:get(State, relname),
  create_dockerfile1(FD, State, ?POSTINSTALL(bucs:to_string(Relname))).

create_dockerfile1(_, _, []) -> ok;
create_dockerfile1(FD, State, [{preinstall, PreInst}|Data]) ->
  application:load(jorel),
  {ok, Vsn} = application:get_key(jorel, vsn),
  create_dockerfile1(FD, State, ?PREINSTALL(Vsn)),
  create_dockerfile1(FD, State, PreInst),
  create_dockerfile1(FD, State, Data);
create_dockerfile1(FD, State, [{postinstall, PostInst}|Data]) ->
  {output_dir, Output} = jorel_config:get(State, output_dir),
  {relname, Relname} = jorel_config:get(State, relname),
  create_dockerfile1(FD, State, ?INSTALL(Output, bucs:to_string(Relname))),
  create_dockerfile1(FD, State, PostInst),
  create_dockerfile1(FD, State, Data);
create_dockerfile1(FD, State, [{label, Key, Value}|Data]) ->
  file:write(FD, io_lib:format("LABEL ~s=\"~s\"~n", [Key, Value])),
  create_dockerfile1(FD, State, Data);
create_dockerfile1(FD, State, [{env, Key, Value}|Data]) ->
  file:write(FD, io_lib:format("ENV ~s=~s~n", [Key, Value])),
  create_dockerfile1(FD, State, Data);
create_dockerfile1(FD, State, [{expose, Ports}|Data]) ->
  file:write(FD, io_lib:format("EXPOSE ~s~n", [string:join([bucs:to_string(E) || E <- Ports], " ")])),
  create_dockerfile1(FD, State, Data);
create_dockerfile1(FD, State, [{Cmd, Paths}|Data]) when Cmd == add; Cmd == copy ->
  file:write(FD, io_lib:format("~s [~s]~n", 
                               [string:to_upper(bucs:to_string(Cmd)),
                                string:join(["\"" ++ bucs:to_string(P) ++ "\"" || P <- Paths], ", ")])),
  create_dockerfile1(FD, State, Data);
create_dockerfile1(FD, State, [{Cmd, Param}|Data]) ->
  file:write(FD, io_lib:format("~s ~s~n", [string:to_upper(bucs:to_string(Cmd)), Param])),
  create_dockerfile1(FD, State, Data).

