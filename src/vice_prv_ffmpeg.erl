% @hidden
-module(vice_prv_ffmpeg).
-compile([{parse_transform, lager_transform}]).

-export([
         init/0
         , infos/2
         , info/3
         , command/5
         , progress/2
        ]).

-record(state, {
          prober,
          converter
         }).

-define(PROBE, "~s -v quiet -of json -show_format -show_streams \"~ts\"").

init() ->
  case vice_utils:find_executable(["ffprobe"], [vice, ffmpeg, ffprobe]) of
    undefined ->
      {stop, {ffprobe, not_found}};
    FFProbe ->
      case vice_utils:find_executable(["ffmpeg"], [vice, ffmpeg, ffmpeg]) of
        undefined ->
          {stop, {ffmpeg, not_found}};
        FFMpeg ->
          {ok, #state{
                  prober = FFProbe,
                  converter = FFMpeg
                 }}
      end
  end.

infos(#state{prober = Prober}, File) ->
  Cmd = lists:flatten(io_lib:format(?PROBE, [Prober, File])),
  case bucos:run(Cmd) of
    {ok, Output} ->
      {ok, jsx:decode(bucs:to_binary(Output), [{labels, atom}, return_maps])};
    Error ->
      Error
  end.

info(State, File, Info) ->
  case infos(State, File) of
    {ok, Infos} ->
      get_info(Infos, Info);
    Error ->
      Error
  end.

get_info(#{format := #{duration := Duration}}, duration) ->
  {ok, bucs:to_float(Duration)};
get_info(#{streams := [#{width := Width}|_]}, width) ->
  {ok, bucs:to_integer(Width)};
get_info(#{streams := [#{height := Height}|_]}, height) ->
  {ok, bucs:to_integer(Height)};
get_info(_, _) ->
  {error, unavailable}.

command(State, In, Out, Options, _Multi) ->
  gen_command(convert, State, In, Out, Options).

gen_command(convert, #state{converter = Converter}, In, Out, Options) ->
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

