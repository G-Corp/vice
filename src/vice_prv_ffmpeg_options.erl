% @hidden
-module(vice_prv_ffmpeg_options).

-export([options/1]).

-export([
    is_true/1,
    is_list_and_list/1,
    is_list_and_integer/1,
    is_list_and_float/1,
    is_list_and_true/1,
    is_integer_and_list/1,
    is_integer_and_integer/1,
    is_integer_and_float/1,
    to_nothing/1,
    to_arg/1,
    to_dotargs/1,
    to_kvarg/1
]).

-define(OPTIONS, [
  {yes,                    global, "-y",                     [{{?MODULE, is_true}, to_nothing}]},
  {fix_sub_duration,       global, "-fix_sub_duration",      [{{?MODULE, is_true}, to_nothing}]},
  {canvas_size,            global, "-canvas_size",           [{{erlang, is_integer}, to_arg}]},
  {filter_complex,         global, "-filter_complex",        [{{erlang, is_list}, to_arg}]},
  {filter_complex_script,  global, "-filter_complex_script", [{{erlang, is_list}, to_arg}]},

  {input_position,         input,  "-ss",                    [{{erlang, is_list}, to_arg}, {{erlang, is_integer}, to_arg}]},
  {input_eof_position,     input,  "-sseof",                 [{{erlang, is_list}, to_arg}, {{erlang, is_integer}, to_arg}]},
  {input_duration,         input,  "-t",                     [{{erlang, is_list}, to_arg}, {{erlang, is_integer}, to_arg}]},
  {decoder,                input,  "-c",                     [{{?MODULE, is_list_and_list}, to_dotargs}]},
  {itoffset,               input,  "-itsoffset",             [{{erlang, is_list}, to_arg}, {{erlang, is_integer}, to_arg}]},
  {input_frame_rate,       input,  "-r",                     [{{?MODULE, is_list_and_integer}, to_dotargs}, {{erlang, is_integer}, to_arg}]},
  {input_frame_size,       input,  "-s",                     [{{?MODULE, is_list_and_list}, to_dotargs}, {{erlang, is_list}, to_arg}]},
  {input_pixel_format,     input,  "-pix_fmt",               [{{?MODULE, is_list_and_list}, to_dotargs}, {{erlang, is_list}, to_arg}]},
  {input_sws_flags,        input,  "-sws_flags",             [{{erlang, is_list}, to_arg}]},
  {input_audio_frequency,  input,  "-ar",                    [{{?MODULE, is_list_and_integer}, to_dotargs}, {{erlang, is_integer}, to_arg}]},
  {input_audio_channels,   input,  "-ac",                    [{{?MODULE, is_list_and_integer}, to_dotargs}, {{erlang, is_integer}, to_arg}]},
  {input_acodec,           input,  "-acodec",                [{{erlang, is_list}, to_arg}]},
  {guess_layout_max,       input,  "-guess_layout_max",      [{{erlang, is_integer}, to_arg}]},
  {input_scodec,           input,  "-scodec",                [{{erlang, is_list}, to_arg}]},
  {muxdelay,               input,  "-muxdelay",              [{{erlang, is_integer}, to_arg}]},
  {muxpreload,             input,  "-muxpreload",            [{{erlang, is_integer}, to_arg}]},
  {accurate_seek,          input,  "-accurate_seek",         [{{?MODULE, is_true}, to_nothing}]},

  {output_format,          output, "-f",                     [{{erlang, is_list}, to_arg}]},
  {output_duration,        output, "-t",                     [{{erlang, is_list}, to_arg}, {{erlang, is_integer}, to_arg}]},
  {output_position,        output, "-ss",                    [{{erlang, is_list}, to_arg}, {{erlang, is_integer}, to_arg}]},
  {output_eof_position,    output,  "-sseof",                [{{erlang, is_list}, to_arg}, {{erlang, is_integer}, to_arg}]},
  {encoder,                output, "-c",                     [{{?MODULE, is_list_and_list}, to_dotargs}, {{erlang, is_list}, to_arg}]},
  {bitrate,                output, "-b",                     [{{?MODULE, is_list_and_list}, to_dotargs}, {{erlang, is_list}, to_arg}, {{erlang, is_integer}, to_arg}]},
  {timestamp,              output, "-timestamp",             [{{erlang, is_list}, to_arg}]},
  {target,                 output, "-target",                [{{erlang, is_list}, to_arg}]},
  {dframes,                output, "-dframes",               [{{erlang, is_integer}, to_arg}]},
  {frames,                 output, "-frames",                [{{?MODULE, is_list_and_integer}, to_dotargs}, {{erlang, is_integer}, to_arg}]},
  {qscale,                 output, "-qscale",                [{{?MODULE, is_list_and_integer}, to_dotargs}, {{erlang, is_integer}, to_arg}]},
  {filter,                 output, "-filter",                [{{?MODULE, is_list_and_integer}, to_dotargs}, {{erlang, is_integer}, to_arg}]},
  {filter_script,          output, "-filter_script",         [{{?MODULE, is_list_and_list}, to_dotargs}, {{erlang, is_list}, to_arg}]}, % TODO: filename
  {pre,                    output, "-pre",                   [{{?MODULE, is_list_and_list}, to_dotargs}, {{erlang, is_list}, to_arg}]},
  {vframes,                output, "-vframes",               [{{erlang, is_integer}, to_arg}]},
  {output_frame_rate,      output, "-r",                     [{{?MODULE, is_list_and_integer}, to_dotargs}, {{erlang, is_integer}, to_arg}]},
  {output_frame_size,      output, "-s",                     [{{?MODULE, is_list_and_list}, to_dotargs}, {{erlang, is_list}, to_arg}]},
  {aspect,                 output, "-aspect",                [{{?MODULE, is_list_and_list}, to_dotargs},
                                                              {{?MODULE, is_list_and_float}, to_dotargs}, {{erlang, is_list}, to_arg}, {{erlang, is_float}, to_arg}]},
  {no_video_recording,     output, "-vn",                    [{{?MODULE, is_true}, to_nothing}]},
  {vcodec,                 output, "-vcodec",                [{{erlang, is_list}, to_arg}]},
  {pass,                   output, "-pass",                  [{{?MODULE, is_list_and_integer}, to_dotargs}, {{erlang, is_integer}, to_arg}]},
  {vlang,                  output, "-vlang",                 [{{erlang, is_list}, to_arg}]},
  {video_filtergraph,      output, "-vf",                    [{{erlang, is_list}, to_arg}]},
  {output_pixel_format,    output, "-pix_fmt",               [{{?MODULE, is_list_and_list}, to_dotargs}, {{erlang, is_list}, to_arg}]},
  {output_sws_flags,       output, "-sws_flags",             [{{erlang, is_list}, to_arg}]},
  {rc_override,            output, "-rc_override",           [{{?MODULE, is_list_and_list}, to_dotargs}, {{erlang, is_list}, to_arg}]},
  {top,                    output, "-top",                   [{{?MODULE, is_list_and_integer}, to_dotargs}, {{erlang, is_integer}, to_arg}]},
  {force_key_frames,       output, "-force_key_frames",      [{{?MODULE, is_list_and_list}, to_dotargs}, {{erlang, is_list}, to_arg}]},
  {copyinkf,               output, "-copyinkf",              [{{?MODULE, is_list_and_true}, to_dotargs}, {{?MODULE, is_true}, to_nothing}]},
  {aframes,                output, "-aframes",               [{{erlang, is_integer}, to_arg}]},
  {output_audio_frequency, output, "-ar",                    [{{?MODULE, is_list_and_integer}, to_dotargs}, {{erlang, is_integer}, to_arg}]},
  {audio_quality,          output, "-aq",                    [{{erlang, is_integer}, to_arg}]},
  {output_audio_channels,  output, "-ac",                    [{{?MODULE, is_list_and_integer}, to_dotargs}, {{erlang, is_integer}, to_arg}]},
  {no_audio_recording,     output, "-an",                    [{{?MODULE, is_true}, to_nothing}]},
  {output_acodec,          output, "-acodec",                [{{erlang, is_list}, to_arg}]},
  {sample_fmt,             output, "-sample_fmt",            [{{?MODULE, is_list_and_list}, to_dotargs}, {{erlang, is_list}, to_arg}]},
  {audio_filtergraph,      output, "-af",                    [{{erlang, is_list}, to_arg}]},
  {output_scodec,          output, "-scodec",                [{{erlang, is_list}, to_arg}]},
  {no_subtitle_recording,  output, "-sn",                    [{{?MODULE, is_true}, to_nothing}]},
  {map,                    output, "-map",                   [{{erlang, is_list}, to_arg}]},
  {map_channel,            output, "-map_channel",           [{{erlang, is_list}, to_arg}]},
  {map_chapters,           output, "-map_chapters",          [{{erlang, is_integer}, to_arg}]},
  {vsync,                  output, "-vsync",                 [{{erlang, is_integer}, to_arg}, {{erlang, is_list}, to_arg}]},
  {async,                  output, "-async",                 [{{erlang, is_integer}, to_arg}]},
  {copytb,                 output, "-copytb",                [{{erlang, is_integer}, to_arg}]},
  {shortest,               output, "-shortest",              [{{?MODULE, is_true}, to_nothing}]},
  {dts_delta_threshold,    output, "-dts_delta_threshold",   [{{?MODULE, is_true}, to_nothing}]},
  {streamid,               output, "-streamid",              [{{erlang, is_list}, to_arg}]},
  {bitstream_filters,      output, "-bsf",                   [{{?MODULE, is_list_and_list}, to_dotargs}, {{erlang, is_list}, to_arg}]},
  {timecode,               output, "-timecode",              [{{erlang, is_list}, to_arg}]},
  {strict,                 output, "-strict",                [{{erlang, is_list}, to_arg}]},
  {metadata,               output, "-metadata",              [{{?MODULE, is_list_and_list}, to_kvarg}]},
  {disable_video,          output, "-vn",                    [{{?MODULE, is_true}, to_nothing}]},
  {disable_audio,          output, "-an",                    [{{?MODULE, is_true}, to_nothing}]},
  {disable_subtitle,       output, "-sn",                    [{{?MODULE, is_true}, to_nothing}]},

  {x264_profile,           output, "-profile",               [{{?MODULE, is_list_and_list}, to_dotargs}]},
  {x264_level,             output, "-level",                 [{{erlang, is_float}, to_arg}]},
  {x264_refs,              output, "-refs",                  [{{erlang, is_integer}, to_arg}]}
]).

options(Options) ->
  lists:foldl(fun({Option, Value}, [{input, Input}, {output, Output}, {global, Global}] = OptionStrings) ->
    case lists:keyfind(Option, 1, ?OPTIONS) of
      false ->
        error_logger:error_msg("Invalid option ~p", [Option]),
        OptionStrings;
      {Option, Level, Param, Validators} ->
        Str = build_option_string(Param, Value, Validators),
        case Level of
          input -> [{input, Input ++ " " ++ Str}, {output, Output}, {global, Global}];
          output -> [{input, Input}, {output, Output ++ " " ++ Str}, {global, Global}];
          global -> [{input, Input}, {output, Output}, {global, Global ++ " " ++ Str}];
          _ ->
            error_logger:error_msg("Invalid level ~p for option ~p", [Level, Option]),
            OptionStrings
        end
    end
  end, [{input, ""}, {output, ""}, {global, ""}], Options).

build_option_string(Param, Value, Validators) ->
  case check_parameter(Value, Validators) of
    {error, _} -> "";
    {ok, StrValue} -> Param ++ StrValue
  end.

check_parameter(_, []) ->
  {error, invalide_type};
check_parameter(Value, [{{GuardModule, GuardFunction}, Formater}|Rest]) ->
  case apply(GuardModule, GuardFunction, [Value]) of
    true ->
      {ok, apply(?MODULE, Formater, [Value])};
    _ ->
      check_parameter(Value, Rest)
  end.

%% @hidden
is_true(true) ->
  true;
is_true(_) ->
  false.
%% @hidden
is_list_and_list([L1, L2]) when is_list(L1), is_list(L2) ->
  true;
is_list_and_list(_) ->
  false.
%% @hidden
is_list_and_integer([L, I]) when is_list(L), is_integer(I) ->
  true;
is_list_and_integer(_) ->
  false.
%% @hidden
is_list_and_float([L, F]) when is_list(L), is_float(F) ->
  true;
is_list_and_float(_) ->
  false.
%% @hidden
is_list_and_true([L, true]) when is_list(L) ->
  true;
is_list_and_true(_) ->
  false.
%% @hidden
is_integer_and_list([I, L]) when is_integer(I), is_list(L) ->
  true;
is_integer_and_list(_) ->
  false.
%% @hidden
is_integer_and_integer([I, L]) when is_integer(I), is_integer(L) ->
  true;
is_integer_and_integer(_) ->
  false.
%% @hidden
is_integer_and_float([I, F]) when is_integer(I), is_float(F) ->
  true;
is_integer_and_float(_) ->
  false.

%% @hidden
to_arg(X) when is_list(X) ->
  " " ++ X;
to_arg(X) when is_integer(X) ->
  " " ++ integer_to_list(X);
to_arg(X) when is_float(X) ->
  " " ++ float_to_list(X).
%% @hidden
to_nothing(_) -> "".
%% @hidden
to_dotargs([X, Y]) when is_integer(X), is_list(Y) ->
  ":" ++ integer_to_list(X) ++ " " ++ Y;
to_dotargs([X, Y]) when is_integer(X), is_integer(Y) ->
  ":" ++ integer_to_list(X) ++ " " ++ integer_to_list(Y);
to_dotargs([X, Y]) when is_integer(X), is_float(Y) ->
  ":" ++ integer_to_list(X) ++ " " ++ float_to_list(Y);
to_dotargs([X, Y]) when is_list(X), is_list(Y) ->
  ":" ++ X ++ " " ++ Y;
to_dotargs([X, Y]) when is_list(X), is_integer(Y) ->
  ":" ++ X ++ " " ++ integer_to_list(Y);
to_dotargs([X, Y]) when is_list(X), is_float(Y) ->
  ":" ++ X ++ " " ++ float_to_list(Y);
to_dotargs([X, true]) when is_list(X) ->
  ":" ++ X.
to_kvarg([K, V]) when is_list(K), is_list(V) ->
  lists:flatten(io_lib:format(" ~s=~p", [K, V])).
