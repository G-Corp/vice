% @hidden
-module(jorel_provider_publish).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, publish).

init(State) ->
  jorel_config:add_provider(
    State,
    {?PROVIDER,
     #{
        module => ?MODULE
        , depends => []
        , desc => "Publish the release"
        % , opts => [
        %          {overwrite, undefined, "overwrite", boolean, "Overwrite version, if exist."}
        %         ]
       }
    }
   ).

do(State) ->
  ?INFO("== Start provider ~p", [?PROVIDER]),
  {relname, RelName} = jorel_config:get(State, relname),
  {relvsn, RelVsn} = jorel_config:get(State, relvsn),
  {outdir, Outdir} = jorel_config:get(State, outdir),
  NameWithVsn = io_lib:format("~s-~s", [RelName, RelVsn]),
  RelArchive = bucfile:expand_path(
                 filename:join([Outdir, "releases", NameWithVsn ++ ".tar.gz"])),
  case filelib:is_file(RelArchive) of
    true ->
      hackney:start(),
      {jorel_in, URL} = jorel_config:get(State, jorel_in, ?JOREL_IN),
      Config = filename:join([?JOREL_HOME, "jorel_in.config"]),
      case filelib:is_file(Config) of
        true ->
          case file:consult(Config) of
            {ok, Term} ->
              Parts = lists:append([{bucs:to_binary(K),
                                     bucs:to_binary(V)}
                                    || {K, V} <- [{name, RelName}|[{version, RelVsn}|Term]]], [{file, bucs:to_binary(RelArchive)}]),
              case hackney:request(post, URL ++ "/api/app/add",
                                   [{<<"Content-Type">>, <<"multipart/form-data">>}],
                                   {multipart, Parts}, []) of
                {ok, 201, _, _} ->
                  ?INFO("* App published", []);
                {ok, 409, _, _} ->
                  ?WARN("* ~s version ~s already exist", [RelName, RelVsn]);
                {ok, Code, _, _} ->
                  ?DEBUG("===> Publication failed: ~p", [Code]),
                  ?ERROR("!!! Publication failed", []);
                Other ->
                  ?DEBUG("===> Publication failed: ~p", [Other]),
                  ?ERROR("!!! Publication failed", [])
              end;
            {error, Reason} ->
              ?ERROR("* Can't read ~s: ~p", [Config, Reason])
          end;
        false ->
          ?ERROR("!!! Please register first", [])
      end;
    _ ->
      ?ERROR("Can't find archive for ~s (~s)", [RelName, RelVsn])
  end,
  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State.

