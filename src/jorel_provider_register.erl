% @hidden
-module(jorel_provider_register).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, register).

init(State) ->
  jorel_config:add_provider(
    State,
    {?PROVIDER,
     #{
       module => ?MODULE,
       depends => [],
       desc => "Register user"
      }
    }
   ).

do(State) ->
  hackney:start(),
  {jorel_in, URL} = jorel_config:get(State, jorel_in, ?JOREL_IN),
  Config = filename:join([?JOREL_HOME, "jorel_in.config"]),
  Email = q("Email"),
  Password = q("Password", true),
  case hackney:request(post, URL ++ "/api/user/register",
                      [{<<"Content-Type">>, <<"application/json">>}],
                      jsx:encode(#{password => bucs:to_binary(Password),
                                   email => bucs:to_binary(Email)}), []) of
    {ok, 201, _, Ref} ->
      case hackney:body(Ref) of
        {ok, Body} ->
          Data = [{K, bucs:to_string(V)} || {K, V} <- jsx:decode(Body, [{labels, atom}])],
          case erlconf:open(config, Config, [{save_on_close, false}]) of
            {ok, _} ->
              {ok, Data} = erlconf:term(config, Data),
              _ = erlconf:save(config),
              _ = erlconf:close(config),
              ?INFO("* ~s registered", [Email]);
            Error ->
              ?DEBUG("===> Registration error: ~p", [Error]),
              ?HALT("!!! Can't create ~s", [Config])
          end;
        {error, Reason} ->
          ?DEBUG("===> Registration error: ~p", [Reason]),
          ?HALT("!!! Registration failed", [])
      end;
    {ok, Code, _, _} ->
      ?DEBUG("===> Registration error: ~p", [Code]),
      ?HALT("!!! Registration failed", []);
    Error ->
      ?DEBUG("===> Registration error: ~p", [Error]),
      ?HALT("!!! Registration failed", [])
  end,
  State.

q(What) ->
  q(What, false).
q(What, Confirm) ->
  case ?ASK(What, [], ": ") of
    [] ->
      ?HALT(What ++ " can not be empty!", []);
    U -> if
           Confirm ->
             case q(What ++ " (confirm)", false) of
               U ->
                 U;
               _ ->
                 ?HALT(What ++ " does not match", [])
             end;
           true ->
             U
         end
  end.

