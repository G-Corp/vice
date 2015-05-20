-module(xrel_provider_artifactory).
-behaviour(xrel_provider).
-include("../include/xrel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, artifactory).

init(State) ->
  ok = application:start(inets),
  {ok, _} = application:ensure_all_started(ssl),
  {artifactory, Data} = xrel_config:get(State, artifactory, []),
  Archive = elists:keyfind(deploy, 1, Data, zip),
  xrel_config:add_provider(
    State,
    {?PROVIDER,
     #{
      module => ?MODULE,
        depends => [Archive],
        desc => "Create an archive (" ++
                eutils:to_string(Archive) ++
                ") and deploy it to artifactory"
       }
    }
   ).

do(State) ->
  ?INFO("== Start provider ~p", [?PROVIDER]),
  {output_dir, Outdir} = xrel_config:get(State, output_dir),
  {relname, RelName} = xrel_config:get(State, relname),
  {relvsn, RelVsn} = xrel_config:get(State, relvsn),
  ZipFile = eutils:to_list(RelName) ++ "-" ++ RelVsn ++ ".tar.gz",
  ZipFull = filename:join(Outdir, ZipFile),
  {artifactory, Data} = xrel_config:get(State, artifactory, []),
  URL = case elists:keyfind(url, 1, Data, os:getenv("ARTIFACTORY_URL")) of
          false -> ?HALT("Missing artifactory url", []);
          U -> U
        end,
  Repository = case elists:keyfind(repository, 1, Data, os:getenv("ARTIFACTORY_REPOSITORY")) of
                 false -> ?HALT("Missing artifactory repository", []);
                 R -> R
               end,
  Username = case elists:keyfind(username, 1, Data, undefined) of
               undefined -> undefined;
               env -> case os:getenv("ARTIFACTORY_USERNAME") of
                        false -> ?HALT("ARTIFACTORY_USERNAME not set", []);
                        L -> L
                      end;
               L -> L
             end,
  Password = case elists:keyfind(password, 1, Data, undefined) of
               undefined -> undefined;
               env -> case os:getenv("ARTIFACTORY_PASSWORD") of
                        false -> ?HALT("ARTIFACTORY_PASSWORD not set", []);
                        P -> P
                      end;
               P -> P
             end,
  AuthHeader = if
                 Username =:= undefined orelse Password =:= undefined -> [];
                 true -> [{"Authorization", "Basic " ++ base64:encode_to_string(Username ++ ":" ++ Password)}]
               end,
  URL1 = URL ++ "/" ++ Repository ++ "/" ++ eutils:to_list(RelName) ++ "/" ++ RelVsn ++ "/" ++ ZipFile,
  Body = case file:read_file(ZipFull) of
           {ok, B} -> B;
           _ -> ?HALT("Can't read tgz file", [])
         end,
  Header = case elists:keyfind(checksum, 1, Data) of
             false -> [{"X-Checksum-Sha1", hexstring(crypto:hash(sha, Body))}|AuthHeader];
             Checksum -> [{"X-Checksum-Deploy", "true"}, {"X-Checksum-Sha1", Checksum}|AuthHeader]
           end,
  ?INFO("Deploy ~s to ~s", [ZipFile, URL1]),
  _ = case httpc:request(put, {URL1, Header, "application/x-gzip", Body}, [{ssl, [{verify, 0}]}], []) of
    {ok, {{_, 201, _}, _, _}} -> ok;
    {ok, {{_, Code, Message}, _, _}} -> ?HALT("Deploy faild HTTP ~p: ~s", [Code, Message]);
    {error, Reason} -> ?HALT("Deploy error: ~p", [Reason])
  end,
  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State.

hexstring(<<X:128/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~128.16.0b", [X])).
