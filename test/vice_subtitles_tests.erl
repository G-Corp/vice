-module(vice_subtitles_tests).
-include_lib("eunit/include/eunit.hrl").

vice_subtitles_test_() ->
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
           {error, {1, 1}},
           vice_subtitles:parse("$$$$"))
    end
   ]}.
