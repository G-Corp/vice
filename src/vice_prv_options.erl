% @hidden
-module(vice_prv_options).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([options/2]).

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

to_dotargs([X, Y]) when is_integer(X), is_list(Y) ->
  ":" ++ bucs:to_string(X) ++ " " ++ Y;
to_dotargs([X, Y]) when is_integer(X), is_integer(Y) ->
  ":" ++ bucs:to_string(X) ++ " " ++ bucs:to_string(Y);
to_dotargs([X, Y]) when is_integer(X), is_float(Y) ->
  ":" ++ bucs:to_string(X) ++ " " ++ bucs:to_string(Y);
to_dotargs([X, Y]) when is_list(X), is_list(Y) ->
  ":" ++ X ++ " " ++ Y;
to_dotargs([X, Y]) when is_list(X), is_integer(Y) ->
  ":" ++ X ++ " " ++ bucs:to_string(Y);
to_dotargs([X, Y]) when is_list(X), is_float(Y) ->
  ":" ++ X ++ " " ++ bucs:to_string(Y);
to_dotargs([X, true]) when is_list(X) ->
  ":" ++ X.

to_kvarg([K, V]) when is_list(K), is_list(V) ->
  lists:flatten(io_lib:format(" ~s=~p", [K, V])).

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
