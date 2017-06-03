% @hidden
-module(vice_prv_subs).

-export([tokenize/1]).

-define(is_digit(S), (S >= $0 andalso S =< $9)).
-define(is_space(S), ((S == $\s) orelse (S == $\t))).
-define(is_cr(S), S == $\r).
-define(is_lf(S), S == $\n).

tokenize(String) ->
  tokenize(String, 1, 1, []).

tokenize([], Line, Column, Tokens) ->
  {ok, Line, Column, lists:reverse(Tokens)};

tokenize([H|String], Line, _Column, Tokens) when ?is_cr(H) ->
  tokenize(String, Line, 1, Tokens);
tokenize([H|String], Line, _Column, Tokens) when ?is_lf(H) ->
  tokenize(String, Line + 1, 1, Tokens);

tokenize([H|_] = String, Line, Column, Tokens) when ?is_space(H) ->
  {Rest, Size} = build_space(String, 0),
  tokenize(Rest, Line, Column + Size, [{space, {Line, Column, Column + Size}, Size}|Tokens]);

tokenize([$-, $-, $>|String], Line, Column, Tokens) ->
  tokenize(String, Line, Column + 3, [{arrow, {Line, Column, Column + 3}}|Tokens]);
tokenize([$:|String], Line, Column, Tokens) ->
  tokenize(String, Line, Column + 1, [{colon, {Line, Column, Column + 1}}|Tokens]);
tokenize([$.|String], Line, Column, Tokens) ->
  tokenize(String, Line, Column + 1, [{period, {Line, Column, Column + 1}}|Tokens]);
tokenize([$,|String], Line, Column, Tokens) ->
  tokenize(String, Line, Column + 1, [{comma, {Line, Column, Column + 1}}|Tokens]);

tokenize([H|_] = String, Line, Column, Tokens) when ?is_digit(H) ->
  {Rest, Data, Size} = build_digit(String, 0, []),
  tokenize(Rest, Line, Column + Size, [{digit, {Line, Column, Column + Size}, Data}|Tokens]);

tokenize(String, Line, Column, Tokens) ->
  {Rest, Data, Size} = build_string(String, 0, []),
  tokenize(Rest, Line, Column + Size, [{string, {Line, Column, Column + Size}, Data}|Tokens]).

build_digit([], Len, Acc) ->
  {[], lists:reverse(Acc), Len};
build_digit([H|String], Len, Acc) when ?is_digit(H) ->
  build_digit(String, Len + 1, [H|Acc]);
build_digit(String, Len, Acc) ->
  {String, lists:reverse(Acc), Len}.

build_string([], Len, Acc) ->
  {[], lists:reverse(Acc), Len};
build_string([H|String], Len, Acc) when not ?is_cr(H) andalso
                                        not ?is_lf(H) andalso
                                        not ?is_space(H) ->
  build_string(String, Len + 1, [H|Acc]);
build_string(String, Len, Acc) ->
  {String, lists:reverse(Acc), Len}.

build_space([], Len) ->
  {[], Len};
build_space([H|String], Len) when ?is_space(H) ->
  build_space(String, Len + 1);
build_space(String, Len) ->
  {String, Len}.

