% @hidden
-module(vice_prv_ffmpeg).
-include_lib("bucs/include/bucs.hrl").
-include("../include/vice_ffmpeg.hrl").
-behaviour(vice_encoder).

-export([
         init/0
         , infos/3
         , info/4
         , command/5
         , progress/2
        ]).

-record(state, {
          ffprobe,
          ffmpeg,
          openssl
         }).

-define(PROBE_AE, "~s -v quiet -of json -show_format -show_streams -allowed_extensions \"~ts\" \"~ts\"").
-define(PROBE, "~s -v quiet -of json -show_format -show_streams \"~ts\"").

init() ->
  case vice_utils:find_tools(record_info(fields, state), ffmpeg) of
    {error, Reason} ->
      {stop, Reason};
    {state, Data} ->
      {ok, ?list_to_record(state, Data)}
  end.

infos(#state{ffprobe = Prober}, File, Options) ->
  {ProbeCmd, ProberParams} = case lists:keyfind(allowed_extensions, 1, Options) of
                               {allowed_extensions, Extensions} ->
                                 {?PROBE_AE, [Prober, Extensions, File]};
                               false ->
                                 {?PROBE, [Prober, File]}
                             end,
  Labels = {labels, buclists:keyfind(labels, 1, Options, atom)},
  Cmd = lists:flatten(io_lib:format(ProbeCmd, ProberParams)),
  case bucos:run(Cmd) of
    {ok, Output} ->
      {ok, jsx:decode(bucs:to_binary(Output), [Labels, return_maps])};
    Error ->
      Error
  end.

info(State, File, Info, Options) ->
  case infos(State, File, Options) of
    {ok, Infos} ->
      get_info(Infos, Info);
    Error ->
      Error
  end.

get_info(#{format := #{duration := Duration}}, duration) ->
  {ok, bucs:to_float(Duration)};
get_info(#{streams := Streams}, width) ->
  get_stream_info(Streams, <<"video">>, width);
get_info(#{streams := Streams}, height) ->
  get_stream_info(Streams, <<"video">>, height);
get_info(#{streams := Streams}, r_frame_rate) ->
  case get_stream_info(Streams, <<"video">>, r_frame_rate) of
    {ok, FPS} ->
      case string:tokens(bucs:to_string(FPS), "/") of
        [N, D|_] ->
          {ok, round(bucs:to_float(N) / bucs:to_float(D))};
        [N] ->
          {ok, bucs:to_integer(N)}
      end;
    Error ->
      Error
  end;
get_info(#{streams := Streams}, avg_frame_rate) ->
  case get_stream_info(Streams, <<"video">>, avg_frame_rate) of
    {ok, FPS} ->
      case string:tokens(bucs:to_string(FPS), "/") of
        [N, D|_] ->
          {ok, round(bucs:to_float(N) / bucs:to_float(D))};
        [N] ->
          {ok, bucs:to_integer(N)}
      end;
    Error ->
      Error
  end;
get_info(_, _) ->
  {error, unavailable}.

get_stream_info([], _, _) ->
  {error, unavailable};
get_stream_info([#{codec_type := Type} = Stream|Rest], Type, Name) ->
  case maps:get(Name, Stream, undefined) of
    undefined ->
      get_stream_info(Rest, Type, Name);
    Value ->
      {ok, Value}
  end;
get_stream_info([_|Rest], Type, Name) ->
  get_stream_info(Rest, Type, Name).

command(State, In, Out, Options, _Multi) ->
  gen_command(convert, State, In, Out, Options).

gen_command(convert, #state{ffmpeg = Converter}, In, Out, Options) ->
  Options1 = buclists:merge_keylists(1, [{yes, true}], Options),
  OutPath = filename:dirname(Out),
  OutFile = filename:basename(Out),
  InFile = bucfile:relative_from(In, OutPath),
  gen_options(Converter, InFile, OutFile, OutPath, Options1).

gen_options(Converter, In, Out, Path, Options) ->
  case hls_options(Options, Path) of
    {ok, Options1} ->
      Params = vice_prv_options:options(?OPTIONS, Options1),
      InputOptions = buclists:keyfind(input, 1, Params, ""),
      OutputOptions = buclists:keyfind(output, 1, Params, ""),
      GlobalOptions = buclists:keyfind(global, 1, Params, ""),
      Cmd = lists:flatten(
              io_lib:format(
                "~s~s~s -i \"~ts\"~s \"~ts\"",
                [Converter, GlobalOptions, InputOptions, In, OutputOptions, Out])),
      case Path of
        "." -> {ok, Cmd};
        <<".">> -> {ok, Cmd};
        _ -> {ok, Path, Cmd}
      end;
    Error ->
      Error
  end.

hls_options(Options, Path) ->
  case lists:keyfind(hls_key_info, 1, Options) of
    false ->
      {ok, Options};
    {hls_key_info, HLSKeyInfo} ->
      IV = proplists:get_value(iv, HLSKeyInfo, iv()),
      case enc_file(Path, proplists:get_value(enc_key, HLSKeyInfo, undefined)) of
        {ok, EncFile} ->
          KeyURI = proplists:get_value(key_uri, HLSKeyInfo, EncFile),
          case enc_key_info_file(Path, KeyURI, EncFile, IV) of
            {ok, EncKeyInfoFile} ->
              {ok, [{hls_key_info_file, EncKeyInfoFile}
                    | lists:keydelete(hls_key_info, 1, Options)]};
            Error ->
              Error
          end;
        Error ->
          Error
      end
  end.

iv() ->
  <<Result:128/bits, _/bits>> = crypto:strong_rand_bytes(16),
  bucbinary:to_hex(Result).

enc_file(_Path, undefined) ->
  {error, missing_hls_enc_key};
enc_file(Path, EncKey) ->
  case filelib:is_regular(EncKey) of
    true ->
      bucfile:relative_from(EncKey, Path);
    false ->
      case size(bucs:to_binary(EncKey)) of
        32 ->
          try
            ok = bucfile:make_dir(Path),
            EncFile = filename:join(Path, "enc.key"),
            ok = file:write_file(EncFile, bucbinary:from_hex(EncKey), [write, binary]),
            {ok, "enc.key"}
          catch
            _:_ ->
              {error, invalid_hls_enc_key}
          end;
        _Other ->
          {error, invalid_hls_enc_key}
      end
  end.

enc_key_info_file(Path, KeyURI, KeyFile, IV) ->
  try
    ok = bucfile:make_dir(Path),
    EncKeyInfoFile = filename:join(Path, "enc.keyinfo"),
    ok = file:write_file(
           EncKeyInfoFile,
           <<
             (bucs:to_binary(KeyURI))/binary, "\n",
             (bucs:to_binary(KeyFile))/binary, "\n",
             (bucs:to_binary(IV))/binary
           >>,
           [write, binary]),
    {ok, "enc.keyinfo"}
  catch
    _:_ ->
      {error, invalid_hls_enc_key}
  end.

progress(Bytes, {D, T, P}) ->
  Duration = case D of
               undefined ->
                 case re:run(Bytes, "Duration: (..):(..):(..\\...)", [{capture, all_but_first, list}]) of
                   {match, [HH0, MM0, SS0]} ->
                     list_to_integer(HH0) * 3600 + list_to_integer(MM0) * 60 + list_to_float(SS0);
                   _ ->
                     undefined
                 end;
               _ ->
                 D
             end,
  Time = case re:run(Bytes, "time=(..):(..):(..\\...)", [{capture, all_but_first, list}]) of
           {match, [HH1, MM1, SS1]} ->
             list_to_integer(HH1) * 3600 + list_to_integer(MM1) * 60 + list_to_float(SS1);
           _ ->
             T
         end,
  case {Duration, Time} of
    {undefined, _} ->
      {undefined, undefined, P};
    {_, undefined} ->
      {Duration, undefined, P};
    {_, _} ->
      {Duration, Time, case Time / Duration * 100 of
                         Percent when Percent > 100 -> 100.0;
                         Percent -> Percent
                       end}
  end.
