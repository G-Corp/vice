% @hidden
-module(jorel_provider_artifactory).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, artifactory).

init(State) ->
  case application:ensure_all_started(inets) of
    {ok, _} -> ok;
    _ -> ?HALT("Can't start inets", [])
  end,
  case application:ensure_all_started(ssl) of
    {ok, _} -> ok;
    _ -> ?HALT("Can't start ssl", [])
  end,
  {artifactory, Data} = jorel_config:get(State, artifactory, []),
  Archive = buclists:keyfind(deploy, 1, Data, zip),
  jorel_config:add_provider(
    State,
    {?PROVIDER,
     #{
      module => ?MODULE,
        depends => [Archive],
        desc => "Create an archive (" ++
                bucs:to_string(Archive) ++
                ") and deploy it to artifactory"
       }
    }
   ).

do(State) ->
  ?INFO("== Start provider ~p", [?PROVIDER]),
  {output_dir, Outdir} = jorel_config:get(State, output_dir),
  {relname, RelName} = jorel_config:get(State, relname),
  {relvsn, RelVsn} = jorel_config:get(State, relvsn),
  ZipFile = bucs:to_list(RelName) ++ "-" ++ RelVsn ++ ".tar.gz",
  ZipFull = filename:join(Outdir, ZipFile),
  {artifactory, Data} = jorel_config:get(State, artifactory, []),
  URL = case buclists:keyfind(url, 1, Data, os:getenv("ARTIFACTORY_URL")) of
          false -> ?HALT("Missing artifactory url", []);
          U -> U
        end,
  Repository = case buclists:keyfind(repository, 1, Data, os:getenv("ARTIFACTORY_REPOSITORY")) of
                 false -> ?HALT("Missing artifactory repository", []);
                 R -> R
               end,
  Username = case buclists:keyfind(username, 1, Data, undefined) of
               undefined -> undefined;
               env -> case os:getenv("ARTIFACTORY_USERNAME") of
                        false -> ?HALT("ARTIFACTORY_USERNAME not set", []);
                        L -> L
                      end;
               L -> L
             end,
  Password = case buclists:keyfind(password, 1, Data, undefined) of
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
  URL1 = URL ++ "/" ++ Repository ++ "/" ++ bucs:to_list(RelName) ++ "/" ++ RelVsn ++ "/" ++ ZipFile,
  Body = case file:read_file(ZipFull) of
           {ok, B} -> B;
           _ -> ?HALT("Can't read tgz file", [])
         end,
  Header = case buclists:keyfind(checksum, 1, Data) of
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
