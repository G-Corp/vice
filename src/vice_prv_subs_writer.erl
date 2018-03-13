% @hidden
-module(vice_prv_subs_writer).
-export([to_string/3, to_file/4]).
-export([webvtt_formater/2, srt_formater/2]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEFAULT_OPTIONS, #{
          segment_repeat_cue => false,
          segment_time => 10,
          segment_filename => "subtitle_%d.vtt",
          from => 0,
          to => undefined,
          duration => undefined
         }).
-define(SRT_FORMAT, "~w~n~s:~s:~s,~s --> ~s:~s:~s,~s~n~s").
-define(WEBVTT_FORMAT, "~w~n~s:~s:~s.~s --> ~s:~s:~s.~s~n~s").
-define(WEBVTT_FORMAT_SETTINGS, "~w~n~s:~s:~s.~s --> ~s:~s:~s.~s ~s~n~s").
-define(X_TIMESTAMP_MAP_FORMAT, "WEBVTT~nX-TIMESTAMP-MAP=LOCAL:~s,MPEGTS:~w").

webvtt_formater(
  Num,
  #{duration := #{from := #{hh := FHH, mm := FMM, ss := FSS, ex := FMS},
                  to := #{hh := THH, mm := TMM, ss := TSS, ex := TMS},
                  settings := Settings},

    text := Text}
 ) ->
  lists:flatten(
    io_lib:format(
      ?WEBVTT_FORMAT_SETTINGS,
      [Num,
       FHH, FMM, FSS, FMS,
       THH, TMM, TSS, TMS,
       webvtt_settings(Settings),
       Text]));
webvtt_formater(Num, Cue) ->
  common_formater(?WEBVTT_FORMAT, Num, Cue).

srt_formater(Num, Cue) ->
  common_formater(?SRT_FORMAT, Num, Cue).

common_formater(
  Format,
  Num,
  #{duration := #{from := #{hh := FHH, mm := FMM, ss := FSS, ex := FMS},
                  to := #{hh := THH, mm := TMM, ss := TSS, ex := TMS}},
    text := Text}
 ) ->
  lists:flatten(
    io_lib:format(
      Format,
      [Num,
       FHH, FMM, FSS, FMS,
       THH, TMM, TSS, TMS,
       Text])).

webvtt_settings(Settings) ->
  string:join(do_webvtt_settings(Settings), " ").

do_webvtt_settings([]) -> [];
do_webvtt_settings([{K, V}|Settings]) ->
  [io_lib:format("~s:~s", [K, V])|do_webvtt_settings(Settings)].

to_string(#{cues := Subs}, srt, Options) ->
  to_subs(Subs, options(Options), {[], -1, 0, undefined}, 1, srt_formater);
to_string(#{cues := Subs}, webvtt, Options) ->
  to_subs(Subs, options(Options), {webvtt_headers(Options), -1, 0, undefined}, 1, webvtt_formater);
to_string(_Subs, _Type, _Options) ->
  {error, invalid_type}.

webvtt_headers(#{x_timestamp_map := {CueTime, MPEG2Time}}) ->
  [lists:flatten(io_lib:format(?X_TIMESTAMP_MAP_FORMAT, [CueTime, MPEG2Time]))];
webvtt_headers(_) ->
  ["WEBVTT"].

to_file(Subs, m3u8, File, Options) ->
  case m3u8_segments(Subs, filename:dirname(File), options(Options)) of
    {ok, Segments, MaxDuration} ->
      file:write_file(
        File,
        lists:flatten(
          io_lib:format(
            "#EXTM3U~n#EXT-X-TARGETDURATION:~p~n#EXT-X-VERSION:3~n#EXT-X-MEDIA-SEQUENCE:0~n#EXT-X-PLAYLIST-TYPE:VOD~n~s~n#EXT-X-ENDLIST",
            [
             vice_prv_stdlib:ceil(MaxDuration),
             string:join(segments(Segments), "\n")
            ])));
    Other ->
      Other
  end;
to_file(Subs, Type, File, Options) ->
  case to_string(Subs, Type, Options) of
    {ok, Data, _, _, _} ->
      file:write_file(File, Data);
    Other ->
      Other
  end.

segments(Segments) ->
  segments(Segments, []).
segments([], Acc) ->
  lists:reverse(Acc);
segments([{Duration, File}|Rest], Acc) ->
  segments(Rest, [lists:flatten(io_lib:format("#EXTINF:~p,~n~s", [Duration, File]))|Acc]).

m3u8_segments(Subs, Path, Options) ->
  m3u8_segments(Subs, Path, Options, 0, 0, 0, undefined, []).
m3u8_segments(Subs, Path, #{segment_time := Duration,
                            segment_filename := Filename,
                            segment_repeat_cue := Repeat} = Options, From, FileNo, MaxDuration, PreviousCue, Acc) ->
  {From0, Duration0} = case {Repeat, PreviousCue} of
                                {true, #{duration := #{duration := ExtraDuration, id := ID}}} ->
                                  {ID,
                                   Duration + vice_prv_stdlib:ceil(ExtraDuration)};
                                _ ->
                                  {From, Duration}
                              end,
  SegmentFile = filename(Filename, FileNo),
  case to_string(Subs, webvtt, #{from => From0, duration => Duration0, x_timestamp_map => maps:get(x_timestamp_map, Options, undefined)}) of
    {ok, _Data, _NewFrom, _SegmentDuration, PreviousCue} ->
      {ok, lists:reverse(Acc), MaxDuration};
    {ok, Data, NewFrom, SegmentDuration, LastCue} ->
      file:write_file(filename:join([Path, SegmentFile]), Data),
      m3u8_segments(
        Subs,
        Path,
        Options,
        NewFrom,
        FileNo + 1,
        case SegmentDuration > MaxDuration of
          true -> SegmentDuration;
          _ -> MaxDuration
        end,
        LastCue,
        [{SegmentDuration, SegmentFile}|Acc]);
    no_data ->
      {ok, lists:reverse(Acc), MaxDuration};
    Other ->
      Other
  end.

to_subs([], _, {[], _, _, _}, _, _) ->
  no_data;
to_subs([], _, {["WEBVTT" ++ _] = Acc, _, _, _}, _, _) when length(Acc) == 1 ->
  no_data;
to_subs([], _, {Acc, ID, Duration, LastCue}, _, _) ->
  {ok, string:join(lists:reverse(Acc), "\n\n"), ID, Duration/1000, LastCue};
to_subs([#{duration := #{from := #{hh := FHH, mm := FMM, ss := FSS, ex := FMS},
                         id := ID,
                         duration := Duration}} = Cue|Rest],
        #{from := Start, duration := MaxDuration} = Options,
        {Acc, CID, TotalDuration, LastCue},
        Num,
        Formater) ->
  case to_ms({FHH, FMM, FSS, FMS}) of
    From when From >= Start andalso (MaxDuration == undefined orelse TotalDuration < MaxDuration) ->
      to_subs(Rest,
              Options,
              {
               [erlang:apply(?MODULE, Formater, [Num, Cue])|Acc],
               ID + bucs:to_integer(Duration * 1000),
               TotalDuration + bucs:to_integer(Duration * 1000),
               Cue
              },
              Num + 1,
              Formater);
    From when From < Start ->
      to_subs(Rest, Options, {Acc, CID, TotalDuration, LastCue}, Num, Formater);
    _ ->
      {ok, string:join(lists:reverse(Acc), "\n\n"), CID, TotalDuration/1000, LastCue}
  end.

options(Options) ->
  Opts = #{from := From,
           to := To,
           duration := Duration} = maps:merge(?DEFAULT_OPTIONS, Options),
  From1 = to_ms(From, 0),
  To1 = to_ms(To),
  Opts#{from => From1,
        to => To1,
        duration => duration(From1, To1, Duration)}.

to_ms(Value, Default) ->
  case to_ms(Value) of
    undefined -> Default;
    Other -> Other
  end.
to_ms(Value) when is_integer(Value) ->
  Value;
to_ms(Value) when is_list(Value) ->
  to_ms(bucs:to_binary(Value));
to_ms(<<HH:2/binary, ":", MM:2/binary, ":", SS:2/binary>>) ->
  bucs:to_integer(HH) * 60 * 60 * 1000 +
  bucs:to_integer(MM) * 60 * 1000 +
  bucs:to_integer(SS) * 1000;
to_ms(<<HH:1/binary, ":", MM:2/binary, ":", SS:2/binary>>) ->
  bucs:to_integer(HH) * 60 * 60 * 1000 +
  bucs:to_integer(MM) * 60 * 1000 +
  bucs:to_integer(SS) * 1000;
to_ms(<<HH:2/binary, ":", MM:2/binary, ":", SS:2/binary, ".", MS/binary>>) ->
  bucs:to_integer(HH) * 60 * 60 * 1000 +
  bucs:to_integer(MM) * 60 * 1000 +
  bucs:to_integer(SS) * 1000 +
  bucs:to_integer(MS);
to_ms(<<HH:1/binary, ":", MM:2/binary, ":", SS:2/binary, ".", MS/binary>>) ->
  bucs:to_integer(HH) * 60 * 60 * 1000 +
  bucs:to_integer(MM) * 60 * 1000 +
  bucs:to_integer(SS) * 1000 +
  bucs:to_integer(MS);
to_ms({HH, MM, SS, MS}) ->
  bucs:to_integer(HH) * 60 * 60 * 1000 +
  bucs:to_integer(MM) * 60 * 1000 +
  bucs:to_integer(SS) * 1000 +
  bucs:to_integer(MS);
to_ms(_) ->
  undefined.

duration(_, undefined, undefined) ->
  undefined;
duration(_, undefined, Duration) ->
  Duration * 1000;
duration(From, To, _) when To > From ->
  To - From;
duration(_, _, _) ->
  undefined.

% %d
% %cNd
to_format(String) ->
  try
    [Start, End] = bucstring:split(String, "%"),
    {true, bucs:to_string(
             case bucs:to_binary(End) of
               <<C:1/binary, N:1/binary, "d", Rest/binary>> ->
                 <<(bucs:to_binary(Start))/binary, "~", N:1/binary, "..", C:1/binary, "B", Rest/binary>>;
               <<C:1/binary, N:2/binary, "d", Rest/binary>> ->
                 <<(bucs:to_binary(Start))/binary, "~", N:2/binary, "..", C:1/binary, "B", Rest/binary>>;
               <<C:1/binary, N:3/binary, "d", Rest/binary>> ->
                 <<(bucs:to_binary(Start))/binary, "~", N:3/binary, "..", C:1/binary, "B", Rest/binary>>;
               <<"d", Rest/binary>> ->
                 <<(bucs:to_binary(Start))/binary, "~B", Rest/binary>>
             end)}
  catch
    _:_ ->
      {false, String}
  end.

filename(Format, N) ->
  case to_format(Format) of
    {true, F} ->
      lists:flatten(io_lib:format(F, [N]));
    {false, F} ->
      lists:flatten(io_lib:format(F, []))
  end.

-ifdef(TEST).
vice_prv_subs_writer_internal_test_() ->
  {setup,
   fun() ->
     ok
   end,
   fun(_) ->
     ok
   end,
   [
     fun() ->
      ?assertEqual({true, "hello_~5..0B.vtt"}, to_format("hello_%05d.vtt")),
      ?assertEqual({true, "hello_~B.vtt"}, to_format("hello_%d.vtt")),
      ?assertEqual({true, "hello_~10..-B.vtt"}, to_format("hello_%-10d.vtt")),
      ?assertEqual({true, "hello_~999...B.vtt"}, to_format("hello_%.999d.vtt")),
      ?assertEqual({false, "hello.vtt"}, to_format("hello.vtt"))
     end
   ]}.
-endif.
