% @hidden
-module(vice_utils).

-export([
         find_executable/2
         , to_hms/1
         , reply/3
         , tile/1
        ]).

-define(DEC(X), $0 + X div 10, $0 + X rem 10).

find_executable([], Alternative) ->
  case doteki:get_env(Alternative, undefined) of
    undefined ->
      undefined;
    AppPath ->
      bucs:to_string(AppPath)
  end;
find_executable([App|Rest] = AppList, Alternative) ->
  case bucs:is_string(AppList) of
    true ->
      find_executable([AppList], Alternative);
    false ->
      case os:find_executable(bucs:to_string(App)) of
        false ->
          find_executable(Rest, Alternative);
        AppPath ->
          AppPath
      end
  end.

to_hms(Duration) ->
  SS = trunc(Duration),
  HH = trunc(SS / 3600),
  SS1 = SS - (HH * 3600),
  MM = trunc(SS1 / 60),
  SS2 = SS1 - (MM * 60),
  [?DEC(HH), $:, ?DEC(MM), $:, ?DEC(SS2)].

reply(sync, From, Response) ->
  gen_server:reply(From, Response);
reply({Fun, Extra}, _, Response) when is_function(Fun, 2) ->
  erlang:apply(Fun, [Response, Extra]);
reply({Fun, Extra}, _, _) when is_function(Fun, 1) ->
  erlang:apply(Fun, [Extra]);
reply(Fun, _, Response) when is_function(Fun, 1) ->
  erlang:apply(Fun, [Response]);
reply(Fun, _, _) when is_function(Fun, 0) ->
  erlang:apply(Fun, []);
reply(_, _, Response) ->
  Response.

tile(Number) ->
  {NNumber, Plus} = renum(Number),
  Lines = lines(NNumber, round(math:sqrt(NNumber))),
  {Lines, columns(NNumber, Plus, Lines)}.

lines(_Number, N) when N =< 0 ->
  1;
lines(Number, N) when Number rem N == 0, Number / N == N ->
  N;
lines(Number, N) when Number rem N == 0 ->
  N;
lines(Number, N) ->
  lines(Number, N - 1).

renum(Number) ->
  renum(Number, 0).

renum(Number, Plus) ->
  SQRT = math:sqrt(Number),
  case round(SQRT) == SQRT of
    true ->
      {Number, Plus};
    false ->
      renum(Number - 1, Plus + 1)
  end.

columns(Number, Plus, Lines) ->
  Extra = Plus div Lines,
  round(Number / Lines) + Extra + if Lines * Extra == Plus -> 0;
                                     true -> 1
                                  end.

