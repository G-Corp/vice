% @hidden
-module(vice_prv_encoder).
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
init({Type, []}) ->
  error_logger:info_msg("~ts encoder not available !", [Type]),
  ignore;
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
  case filelib:is_regular(File) of
    true ->
      try
        Reply = erlang:apply(Encoder, infos, [EncoderState, File, Options]),
        {reply, Reply, State}
      catch
        _:_ ->
          {reply, {error, infos_not_availables}, State}
      end;
    false ->
      {reply, {error, file_not_found}, State}
  end;
% @hidden
handle_call({info, File, Info, Options}, _, #state{encoder = Encoder,
                                                   state = EncoderState} = State) ->
  case filelib:is_regular(File) of
    true ->
      try
        Reply = erlang:apply(Encoder, info, [EncoderState, File, Info, Options]),
        {reply, Reply, State}
      catch
        _:_ ->
          {reply, {error, info_not_availables}, State}
      end;
    false ->
      {reply, {error, file_not_found}, State}
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
      RunOptions = get_exec_options(Options),
      GlobalParams = proplists:get_value(global_params, NOptions, []),
      CNOptions = vice_prv_options:compile(lists:keydelete(global_params, 1, NOptions), GlobalParams),
      case erlang:apply(Encoder, command, [EncoderState, In, Out, CNOptions, Multi]) of
        {ok, Path, Cmd} ->
          case bucfile:make_dir(Path) of
            ok ->
              run_command(From, Encoder, Cmd, [{cd, bucs:to_string(Path)}|RunOptions], Fun, In, Out);
            Error ->
              vice_utils:reply(Fun, From, Error)
          end;
        {ok, Cmd} ->
          run_command(From, Encoder, Cmd, RunOptions, Fun, In, Out);
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

get_exec_options(Options) ->
  buclists:delete_if(fun
                       ({cgroup, _}) -> false;
                       ({cgexec, _}) -> false;
                       ({cmd_options, _}) -> false;
                       (_) -> true
                     end, Options).

run_command(From, Encoder, Cmd, Options, Fun, In, Out) ->
  CmdOptions = proplists:get_value(cmd_options, Options, erlang:make_ref()),
  Ref = vice_prv_status:insert(CmdOptions, self()),
  case Fun of
    sync ->
      ok;
    _ ->
      gen_server:reply(From, {async, Ref})
  end,
  case vice_command:exec(Cmd, proplists:delete(cmd_options, Options), Encoder, Ref) of
    {ok, _} ->
      vice_utils:reply(Fun, From, {ok, In, Out});
    {error, Code} ->
      vice_utils:reply(Fun, From, {error, In, Out, Code})
  end,
  vice_prv_status:delete(Ref).

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
      error_logger:info_msg("Preset file : ~s", [File]),
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
  error_logger:info_msg("Use preset ~s : ~s", [PresetName, PresetDesc]),
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
