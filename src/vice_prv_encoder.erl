% @hidden
-module(vice_prv_encoder).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1
         , handle_call/3
         , handle_cast/2
         , handle_info/2
         , terminate/2
         , code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          encoder,
          state
         }).

start_link(Encoders) ->
  gen_server:start_link(?MODULE, Encoders, []).

% @hidden
init([]) ->
  {stop, missing_encoder};
init([Encoder|Rest]) ->
  case erlang:apply(Encoder, init, []) of
    {ok, State} ->
      {ok, #state{encoder = Encoder, state = State}};
    _ ->
      init(Rest)
  end.

% @hidden
handle_call({infos, File}, _, #state{encoder = Encoder,
                                     state = EncoderState} = State) ->
  try
    Reply = erlang:apply(Encoder, infos, [EncoderState, File]),
    {reply, Reply, State}
  catch
    _:_ ->
      {reply, {error, infos_not_availables}, State}
  end;
% @hidden
handle_call({info, File, Info}, _, #state{encoder = Encoder,
                                          state = EncoderState} = State) ->
  try
    Reply = erlang:apply(Encoder, info, [EncoderState, File, Info]),
    {reply, Reply, State}
  catch
    _:_ ->
      {reply, {error, infos_not_availables}, State}
  end;
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

% @hidden
handle_cast({convert, In, Out, Options, Multi, Fun, From}, #state{encoder = Encoder,
                                                                  state = EncoderState} = State) ->
  case erlang:apply(Encoder, command, [EncoderState, In, Out, Options, Multi]) of
    {ok, Cmd} ->
      lager:debug("COMMAND : ~p", [Cmd]),
      Ref = erlang:make_ref(),
      vice_prv_status:insert(Ref, self()),
      case Fun of
        sync ->
          ok;
        _ ->
          gen_server:reply(From, {async, Ref})
      end,
      case vice_command:exec(Cmd, Encoder, Ref) of
        {ok, _, _} ->
          vice_utils:reply(Fun, From, {ok, In, Out});
        {error, Code, _} ->
          vice_utils:reply(Fun, From, {error, Code})
      end,
      vice_prv_status:delete(Ref);
    Error ->
      vice_utils:reply(Fun, From, Error)
  end,
  gen_server:cast(vice, {terminate, self()}),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info(_Info, State) ->
  {noreply, State}.

% @hidden
terminate(_Reason, _State) ->
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

