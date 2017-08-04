% @hidden
-module(vice_prv_subs_writer).
-export([to_string/3, to_file/4]).

-define(DEFAULT_SEGMENTS, #{
          segment_time => 2,
          segment_filename => "subtitle_%d.vtt",
          from => 0,
          to => undefined,
          duration => undefined
         }).

to_string(#{cues := Subs}, srt, Options) ->
  to_srt(Subs, options(Options), {[], -1, 0}, 1);

to_string(_Subs, _Type, _Options) ->
  {error, invalid_type}.

to_file(_Subs, _Type, _File, _Options) ->
  todo.

to_srt([], _, {Acc, ID, _}, _) ->
  {ok, string:join(lists:reverse(Acc), "\n\n"), ID};
to_srt([#{duration := #{from := #{hh := FHH, mm := FMM, ss := FSS, ex := FMS},
                        to := #{hh := THH, mm := TMM, ss := TSS, ex := TMS},
                        id := ID,
                        duration := Duration},
          text := Text}|Rest],
       #{from := Start, duration := MaxDuration} = Options,
       {Acc, CID, TotalDuration}, Num) ->
  case to_ms({FHH, FMM, FSS, FMS}) of
    From when From >= Start andalso (MaxDuration == undefined orelse TotalDuration < MaxDuration) ->
      to_srt(Rest,
             Options,
             {
              [lists:flatten(
                 io_lib:format(
                   "~w~n~s:~s:~s,~s --> ~s:~s:~s,~s~n~s",
                   [Num,
                    FHH, FMM, FSS, FMS,
                    THH, TMM, TSS, TMS,
                    Text]))
               |Acc],
              ID + bucs:to_integer(Duration * 1000),
              TotalDuration  + bucs:to_integer(Duration * 1000)
             },
             Num + 1);
    From when From < Start ->
      to_srt(Rest, Options, {Acc, CID, TotalDuration}, Num);
    _ ->
      {ok, string:join(lists:reverse(Acc), "\n\n"), CID}
  end.

options(Options) ->
  Opts = #{from := From,
           to := To,
           duration := Duration} = maps:merge(?DEFAULT_SEGMENTS, Options),
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

