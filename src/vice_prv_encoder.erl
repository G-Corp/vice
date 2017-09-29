% @hidden
-module(vice_prv_encoder).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1
         , handle_call/3
         , handle_cast/2
         , handle_info/2
         , terminate/2
         , code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          type,
          encoder,
          state
         }).

start_link(Type, Encoders) ->
  gen_server:start_link(?MODULE, {Type, Encoders}, []).

% @hidden
init({_, []}) ->
  {stop, missing_encoder};
init({Type, [Encoder|Rest]}) ->
  case erlang:apply(Encoder, init, []) of
    {ok, State} ->
      {ok, #state{type = Type,
                  encoder = Encoder,
                  state = State}};
    _ ->
      init({Type, Rest})
  end.

% @hidden
handle_call({infos, File, Options}, _, #state{encoder = Encoder,
                                     state = EncoderState} = State) ->
  try
    Reply = erlang:apply(Encoder, infos, [EncoderState, File, Options]),
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
      {reply, {error, info_not_availables}, State}
  end;
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

% @hidden
handle_cast({convert, In, Out, Options, Multi, Fun, From}, #state{type = Type,
                                                                  encoder = Encoder,
                                                                  state = EncoderState} = State) ->
  case update_with_preset(Options, Type) of
    {ok, NOptions} ->
      case erlang:apply(Encoder, command, [EncoderState, In, Out, NOptions, Multi]) of
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
              vice_utils:reply(Fun, From, {error, In, Out, Code})
          end,
          vice_prv_status:delete(Ref);
        Error ->
          vice_utils:reply(Fun, From, Error)
      end;
    {error, Reason} ->
      gen_server:reply(From, {error, Reason})
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


update_with_preset(Options, Type) ->
  case get_preset_file(Options, Type) of
    undefined ->
      {ok, Options};
    File ->
      lager:info("Preset file : ~s", [File]),
      case file:consult(File) of
        {ok, [List]} when is_list(List) ->
          update_options(Options, List);
        {ok, List} when is_list(List) ->
          update_options(Options, List);
        _ ->
          {error, invalid_preset}
      end
  end.

update_options(Options, Preset) ->
  PresetDesc = buclists:keyfind(description, 1, Preset, "<no description>"),
  PresetName = buclists:keyfind(name, 1, Preset, "<no name>"),
  Presets = buclists:keyfind(options, 1, Preset, []),
  lager:info("Use preset ~s : ~s", [PresetName, PresetDesc]),
  merge_options(Presets, lists:keydelete(preset, 1, Options)).

merge_options([], Options) ->
  {ok, Options};
merge_options([Preset|Rest], Options) when is_tuple(Preset) ->
  case lists:keymember(erlang:element(1, Preset), 1, Options) of
    true ->
      merge_options(Rest, Options);
    false ->
      merge_options(Rest, [Preset|Options])
  end;
merge_options([Preset|Rest], Options) ->
  case lists:member(Preset, Options) of
    true ->
      merge_options(Rest, Options);
    false ->
      merge_options(Rest, [Preset|Options])
  end.

get_preset_file(Options, Type) ->
  case lists:keyfind(preset, 1, Options) of
    {preset, Name} when is_atom(Name) ->
      filename:join([buccode:priv_dir(vice),
                     "presets",
                     Type,
                     Name]) ++ ".preset";
    {preset, Name} ->
      case filelib:is_regular(Name) of
        true -> Name;
        false ->
          filename:join([buccode:priv_dir(vice),
                         "presets",
                         Type,
                         Name]) ++ ".preset"
      end;
    false ->
      undefined
  end.

