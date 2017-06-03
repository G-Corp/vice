% @hidden
-module(vice_prv_subs).

-export([tokenize/1]).

-define(IS_DIGIT(S), (S >= $0 andalso S =< $9)).
-define(IS_SPACE(S), ((S == $\s) orelse (S == $\t))).
-define(IS_CR(S), S == $\r).
-define(IS_LF(S), S == $\n).

tokenize(String) ->
  tokenize(String, 1, 1, []).

tokenize([], Line, Column, Tokens) ->
  {ok, Line, Column, lists:reverse(Tokens)};

tokenize([H|String], Line, _Column, Tokens) when ?IS_CR(H) ->
  tokenize(String, Line, 1, Tokens);
tokenize([H|String], Line, _Column, Tokens) when ?IS_LF(H) ->
  tokenize(String, Line + 1, 1, Tokens);

tokenize([H|_] = String, Line, Column, Tokens) when ?IS_SPACE(H) ->
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

tokenize([H|_] = String, Line, Column, Tokens) when ?IS_DIGIT(H) ->
  {Rest, Data, Size} = build_digit(String, 0, []),
  tokenize(Rest, Line, Column + Size, [{digit, {Line, Column, Column + Size}, Data}|Tokens]);

tokenize(String, Line, Column, Tokens) ->
  {Rest, Data, Size} = build_string(String, 0, []),
  tokenize(Rest, Line, Column + Size, [{string, {Line, Column, Column + Size}, Data}|Tokens]).

build_digit([], Len, Acc) ->
  {[], lists:reverse(Acc), Len};
build_digit([H|String], Len, Acc) when ?IS_DIGIT(H) ->
  build_digit(String, Len + 1, [H|Acc]);
build_digit(String, Len, Acc) ->
  {String, lists:reverse(Acc), Len}.

build_string([], Len, Acc) ->
  {[], lists:reverse(Acc), Len};
build_string([H|String], Len, Acc) when not(?IS_CR(H)) andalso
                                        not(?IS_LF(H)) andalso
                                        not(?IS_SPACE(H)) ->
  build_string(String, Len + 1, [H|Acc]);
build_string(String, Len, Acc) ->
  {String, lists:reverse(Acc), Len}.

build_space([], Len) ->
  {[], Len};
build_space([H|String], Len) when ?IS_SPACE(H) ->
  build_space(String, Len + 1);
build_space(String, Len) ->
  {String, Len}.

