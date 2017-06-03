-module(vice_prv_subs_tests).
-include_lib("eunit/include/eunit.hrl").

vice_prv_subs_tests_test_() ->
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
           {ok, 1, 2, [{digit, {1, 1, 2}, "1"}]},
           vice_prv_subs:tokenize("1")),
        ?assertEqual(
           {ok, 1, 6, [{string, {1, 1, 6}, "Hello"}]},
           vice_prv_subs:tokenize("Hello"))
    end
   ]}.
