% @hidden
-module(vice_prv_options).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([options/2, compile/2]).

-export([
    is_true/1,
    is_in/2,
    is_list_and_list/1,
    is_list_and_integer/1,
    is_list_and_float/1,
    is_list_and_true/1,
    is_integer_and_list/1,
    is_integer_and_integer/1,
    is_integer_and_float/1,
    to_nothing/1,
    to_arg/1,
    to_karg/1,
    to_params_string/1,
    to_quote_arg/1,
    to_equal_arg/1,
    to_dotargs/1,
    to_kvarg/1
]).

options(OptionsDef, Options) ->
  lists:foldl(fun
                ({Option, Value}, OptionStrings) ->
                  case lists:keyfind(Option, 1, OptionsDef) of
                    false ->
                      error_logger:error_msg("Invalid option ~p", [Option]),
                      OptionStrings;
                    {Option, Level, Param, Validators} ->
                      Str = build_option_string(Param, Value, Validators),
                      Current = buclists:keyfind(Level, 1, OptionStrings, ""),
                      buclists:keyupdate(Level, 1, OptionStrings, {Level, Current ++ " " ++ Str})
                  end;
                ({Option, N, Value}, OptionStrings) ->
                  case lists:keyfind(Option, 1, OptionsDef) of
                    false ->
                      error_logger:error_msg("Invalid option ~p", [Option]),
                      OptionStrings;
                    {Option, Level, Param, Validators} ->
                      Str = build_option_string(Param, Value, Validators),
                      Currents0 = case buclists:keyfind(Level, 1, OptionStrings, "") of
                                    [X|_] = Currents ->
                                      case is_list(X) of
                                        true -> Currents;
                                        false -> [Currents]
                                      end;
                                    Currents ->
                                      [Currents]
                                  end,
                      buclists:keyupdate(Level, 1, OptionStrings, {Level, append_to(Currents0, N, Str)})
                  end
              end, [], Options).

append_to(List, Position, String) ->
  append_to(List, Position, String, []).

append_to([], 0, _, Result) ->
  lists:reverse(Result);
append_to([Current|List], 0, String, Result) ->
  append_to(List, 0, String, [Current|Result]);

append_to([], 1, String, Result) ->
  append_to([], 0, String, [String|Result]);
append_to([Current|List], 1, String, Result) ->
  append_to(List, 0, String, [Current ++ " " ++ String|Result]);

append_to([], N, String, Result) when N > 1 ->
  append_to([], N - 1, String, [""|Result]);
append_to([Current|List], N, String, Result) when N > 1 ->
  append_to(List, N - 1, String, [Current|Result]).

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
      {ok, apply(vice_prv_options, Formater, [Value])};
    _ ->
      check_parameter(Value, Rest)
  end;
check_parameter(Value, [{{GuardModule, GuardFunction, Args}, Formater}|Rest]) ->
  case apply(GuardModule, GuardFunction, [Value, Args]) of
    true ->
      {ok, apply(vice_prv_options, Formater, [Value])};
    _ ->
      check_parameter(Value, Rest)
  end.

is_true(true) ->
  true;
is_true(_) ->
  false.

is_in(Value, List) ->
  lists:member(Value, List).

is_list_and_list([L1, L2]) when is_list(L1), is_list(L2) ->
  true;
is_list_and_list(_) ->
  false.

is_list_and_integer([L, I]) when is_list(L), is_integer(I) ->
  true;
is_list_and_integer(_) ->
  false.

is_list_and_float([L, F]) when is_list(L), is_float(F) ->
  true;
is_list_and_float(_) ->
  false.

is_list_and_true([L, true]) when is_list(L) ->
  true;
is_list_and_true(_) ->
  false.

is_integer_and_list([I, L]) when is_integer(I), is_list(L) ->
  true;
is_integer_and_list(_) ->
  false.

is_integer_and_integer([I, L]) when is_integer(I), is_integer(L) ->
  true;
is_integer_and_integer(_) ->
  false.

is_integer_and_float([I, F]) when is_integer(I), is_float(F) ->
  true;
is_integer_and_float(_) ->
  false.

to_arg(X) ->
  " " ++ bucs:to_string(X).

to_karg(X) ->
  " " ++ bucs:to_string(X) ++ "k".

to_equal_arg(X) ->
  "=" ++ bucs:to_string(X).

to_quote_arg(X) ->
  "\"" ++ bucs:to_string(X) ++ "\"".

to_nothing(_) -> "".

to_dotargs(Args) when is_tuple(Args) ->
  to_dotargs(bucs:to_list(Args));
to_dotargs(Args) when is_list(Args) ->
  lists:flatten(do_to_dotargs(Args)).
do_to_dotargs([X, true]) ->
  [":", bucs:to_string(X)];
do_to_dotargs([X, Y]) ->
  [":", bucs:to_string(X), " ", bucs:to_string(Y)];
do_to_dotargs([X|Rest]) ->
  [":", bucs:to_string(X) | to_dotargs(Rest)].

to_params_string(List) when is_list(List) ->
  " " ++ string:join(
           [bucs:to_string(Key) ++ "=" ++ bucs:to_string(Value) || {Key, Value} <- List],
           ":").

to_kvarg([K, V]) when is_list(K), is_list(V) ->
  lists:flatten(io_lib:format(" ~s=~p", [K, V])).

compile(Preset, []) ->
  Preset;
compile(Preset, Data) when is_list(Data) ->
  compile(Preset, normalize(Data), []);
compile(Preset, Data) when is_map(Data) ->
  compile(Preset, bucmaps:to_list(Data)).

compile([], _Data, Acc) ->
  lists:reverse(Acc);
compile([Element|Rest], Data, Acc) ->
  compile(Rest, Data, update(Element, Data, Acc)).

normalize([]) -> [];
normalize([Tuple|Data]) when is_tuple(Tuple), not is_atom(element(1, Tuple)) ->
  [setelement(1, Tuple, bucs:to_atom(element(1, Tuple)))|normalize(Data)];
normalize([Element|Data]) ->
  [Element|normalize(Data)].

update(Element, Data, Acc) when is_tuple(Element) ->
  case marker(Element) of
    {true, Marker} ->
      [evaluate(Marker, Data)|Acc];
    false ->
      [compile_tuple(Element, Data)|Acc]
  end;
update(Element, Data, Acc) when is_list(Element) ->
  case bucs:is_string(Element) of
    true ->
      [compile_string(Element, Data)|Acc];
    false ->
      [compile(Element, Data)|Acc]
  end;
update(Element, Data, Acc) when is_binary(Element) ->
  [compile_string(Element, Data)|Acc];
update(Element, _Data, Acc) ->
  [Element|Acc].

marker({{Marker}}) when is_atom(Marker) ->
  {true, Marker};
marker(_Element) ->
  false.

evaluate(Marker, Data) ->
  case proplists:get_value(Marker, Data, '$undefined$') of
    '$undefined$' ->
      do_evaluate(Marker, Data);
    Value ->
      Value
  end.
do_evaluate(Marker, Data) ->
  Expr = lists:foldl(fun
                       ({Key, Value}, Acc) when is_integer(Value); is_float(Value) ->
                         binary:replace(Acc, bucs:to_binary(Key), bucs:to_binary(Value), [global]);
                       (_, Acc) ->
                         Acc
                     end, bucs:to_binary(Marker), Data),
  try
    {ok, Tokens, _} = erl_scan:string(bucs:to_string(<<Expr/binary, ".">>)),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:exprs(Parsed, []),
    Result
  catch
    _:_ -> undefined
  end.

compile_tuple(Tuple, Data) ->
  list_to_tuple(compile(tuple_to_list(Tuple), Data)).

compile_string(String, Data) ->
  case re:run(String, "{{[^{}]*}}", [global, {capture, all, binary}]) of
    nomatch ->
      String;
    {match, Matchs} ->
      compile_string(
        evaluate_string_markers(lists:flatten(Matchs), String, Data),
        Data)
  end.

evaluate_string_markers([], String, _Data) ->
  String;
evaluate_string_markers([Marker|Rest], String, Data) ->
  Value = evaluate(marker_to_atom(Marker), Data),
  Result = binary:replace(bucs:to_binary(String), Marker, bucs:to_binary(Value)),
  evaluate_string_markers(
    Rest,
    case is_list(String) of
      true ->
        bucs:to_string(Result);
      false ->
        Result
    end,
    Data).


marker_to_atom(M) ->
  Size = size(M) - 4,
  <<"{{", N:Size/binary, "}}">> = M,
  bucs:to_atom(bucbinary:trim(N, both)).

-ifdef(TEST).
vice_prv_options_internal_test_() ->
  {setup,
   fun() ->
     ok
   end,
   fun(_) ->
     ok
   end,
   [
     fun() ->
         ?assertEqual(
            ["", "", "hello"],
            append_to([], 3, "hello")),
         ?assertEqual(
            ["hello"],
            append_to([], 1, "hello")),
         ?assertEqual(
            ["1", "2", "3 hello", "4", "5"],
            append_to(["1", "2", "3", "4", "5"], 3, "hello"))
     end
   ]}.
-endif.
