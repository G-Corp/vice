% @hidden
-module(vice_prv_ffmpeg).
-compile([{parse_transform, lager_transform}]).
-include_lib("bucs/include/bucs.hrl").

-export([
         init/0
         , infos/3
         , info/3
         , command/5
         , progress/2
        ]).

-record(state, {
          ffprobe,
          ffmpeg,
          openssl
         }).

-define(PROBE, "~s -v quiet -of json -show_format -show_streams \"~ts\"").

init() ->
  case vice_utils:find_tools(record_info(fields, state)) of
    {error, Reason} ->
      {stop, Reason};
    {state, Data} ->
      {ok, ?list_to_record(state, Data)}
  end.

infos(#state{ffprobe = Prober}, File, Options) ->
  Labels = {labels, buclists:keyfind(labels, 1, Options, atom)},
  Cmd = lists:flatten(io_lib:format(?PROBE, [Prober, File])),
  case bucos:run(Cmd) of
    {ok, Output} ->
      {ok, jsx:decode(bucs:to_binary(Output), [Labels, return_maps])};
    Error ->
      Error
  end.

info(State, File, Info) ->
  case infos(State, File, []) of
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
  {ok, gen_options(Converter, In, Out, Options1)}.

gen_options(Converter, In, Out, Options) ->
  [
    {input, InputOptions},
    {output, OutputOptions},
    {global, GlobalOptions}
  ] = vice_prv_ffmpeg_options:options(Options),
  lists:flatten(
    io_lib:format(
      "~s~s~s -i \"~ts\"~s \"~ts\"",
      [Converter, GlobalOptions, InputOptions, In, OutputOptions, Out])).

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

