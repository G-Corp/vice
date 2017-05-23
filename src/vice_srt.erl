-module(vice_srt).

-export([parse/1, parse_file/1, write/2, to_string/1]).

-define(NEW_ENTRY, #{num => undefined,
                     from => undefined,
                     to => undefined,
                     lines => []}).

parse_file(File) ->
  case file:read_file(File) of
    {ok, Data} ->
      parse(Data);
    Error ->
      Error
  end.

parse(Data) ->
  parse(lines(Data), start, undefined, 1, []).

parse([], string, Current, L, Acc) ->
  case verify_and_add(Current, Acc) of
    {ok, _, Acc0} ->
      {ok, lists:reverse(Acc0)};
    _ ->
      {error, missing_data1, {line, L - 1}}
  end;
parse([], {empty, N}, Current, L, Acc) ->
  case verify_and_add(add_empty(N - 2, Current), Acc) of
    {ok, _, Acc0} ->
      {ok, lists:reverse(Acc0)};
    _ ->
      {error, missing_data2, {line, L - 1}}
  end;

parse([<<"WEBVTT", _/binary>>|Rest], start, Current, L, Acc) ->
  parse(Rest, header, Current, L + 1, Acc);

parse([<<>>|Rest], header, Current, L, Acc) ->
  parse(Rest, {empty, 1}, Current, L + 1, Acc);
parse([<<>>|Rest], {empty, N}, Current, L, Acc) ->
  parse(Rest, {empty, N + 1}, Current, L + 1, Acc);
parse([<<>>|Rest], string, Current, L, Acc) ->
  parse(Rest, {empty, 1}, Current, L + 1, Acc);
parse([<<>>|Rest], duration, Current, L, Acc) ->
  parse(Rest, {empty, 1}, Current, L + 1, Acc);

parse([Data|Rest], {empty, N}, Current, L, Acc) ->
  Current0 = add_empty(N - 1, Current),
  case re:run(Data, "^([0-9]+)$", [global, {capture, all_but_first, list}]) of
    {match, [[Num]]} ->
      case verify_and_add(Current0, Acc) of
        {ok, Current1, Acc0} ->
          parse(Rest, number, Current1#{num => bucs:to_integer(Num)}, L + 1, Acc0);
        _ ->
          {error, missing_data3, {line, L}}
      end;
    nomatch ->
      parse_duration(Data, Rest, Current0, L, Acc, true)
  end;

parse([Data|Rest], number, Current, L, Acc) ->
  parse_duration(Data, Rest, Current, L, Acc, false);

parse([Data|Rest], duration, #{lines := Lines} = Current, L, Acc) ->
  parse(Rest, string, Current#{lines => [Data|Lines]}, L + 1, Acc);

parse([Data|Rest], string, #{lines := Lines} = Current, L, Acc) ->
  parse(Rest, string, Current#{lines => [Data|Lines]}, L + 1, Acc);

parse(_, _, _, L, _) ->
  {error, missing_data4, {line, L}}.

add_empty(N, #{lines := Lines} = Current) when N > 0 ->
  add_empty(N - 1, Current#{lines => [<<>>|Lines]});
add_empty(_, Current) ->
  Current.

parse_duration(Data, Rest, Current, L, Acc, CanBeString) ->
  case re:run(Data, "^((\\d\\d):)?(\\d\\d):(\\d\\d),(\\d\\d\\d)\s+-->\s+((\\d\\d):)?(\\d\\d):(\\d\\d),(\\d\\d\\d)", [global, {capture, all_but_first, list}]) of
    {match, [[_, SHH, SMM, SSS, SMS, _, EHH, EMM, ESS, EMS]]} ->
      case verify_and_add(Current, Acc) of
        {ok, Current0, Acc0} ->
          parse(Rest, duration, Current0#{from => {SHH, SMM, SSS, SMS},
                                          to => {EHH, EMM, ESS, EMS}}, L + 1, Acc0);
        _ ->
          {error, missing_data5, {line, L}}
      end;
    nomatch ->
      case {Current, CanBeString} of
        {_, false} ->
          {error, missing_duration, {line, L}};
        {#{lines := Lines}, true} ->
          parse(Rest, string, Current#{lines => [Data|Lines]}, L + 1, Acc);
        _ ->
          {error, cant_be_string, {line, L}}
      end
  end.

verify_and_add(#{num := _,
                 from := {_, _, _, _},
                 to := {_, _, _, _},
                 lines := Lines} = Current, Acc) when is_list(Lines) ->
  {ok, ?NEW_ENTRY, [Current#{lines => lists:reverse(Lines)}|Acc]};
verify_and_add(undefined, Acc) ->
  {ok, ?NEW_ENTRY, Acc};
verify_and_add(#{num := N} = Current, Acc) when is_integer(N) ->
  {ok, Current, Acc};
verify_and_add(_, _) ->
  error.

write(_File, _Subtitles) ->
  todo. % TODO

to_string(_Subtitles) ->
  todo. % TODO

lines(Data) ->
  lines(bucs:to_string(Data), [], []).
lines([], Line, Lines) ->
  lists:reverse([line(Line)|Lines]);
lines([$\r, $\n|Rest], Line, Lines) ->
  lines(Rest, [], [line(Line)|Lines]);
lines([$\n|Rest], Line, Lines) ->
  lines(Rest, [], [line(Line)|Lines]);
lines([C|Rest], Line, Lines) ->
  lines(Rest, [C|Line], Lines).
line(Data) ->
  bucs:to_binary(lists:reverse(Data)).

