-module(vice_utils_tests).
-include_lib("eunit/include/eunit.hrl").

hms_test_() ->
  {setup,
   fun() ->
       ok
   end,
   fun(_) ->
       ok
   end,
   [
    fun() ->
        ?assertEqual("00:00:01", vice_utils:to_hms(1)),
        ?assertEqual("00:00:10", vice_utils:to_hms(10)),
        ?assertEqual("00:01:00", vice_utils:to_hms(60)),
        ?assertEqual("00:10:00", vice_utils:to_hms(600)),
        ?assertEqual("10:00:00", vice_utils:to_hms(36000))
    end,
    fun() ->
        ?assertEqual("00:00:01", vice_utils:to_hms(1.59)),
        ?assertEqual("00:00:01.590", vice_utils:to_full_hms(1.59)),
        ?assertEqual("00:00:10", vice_utils:to_hms(10.59)),
        ?assertEqual("00:00:10.590", vice_utils:to_full_hms(10.59)),
        ?assertEqual("00:01:00", vice_utils:to_hms(60.59)),
        ?assertEqual("00:01:00.590", vice_utils:to_full_hms(60.59)),
        ?assertEqual("00:10:00", vice_utils:to_hms(600.59)),
        ?assertEqual("00:10:00.590", vice_utils:to_full_hms(600.59)),
        ?assertEqual("01:00:00", vice_utils:to_hms(3600.59)),
        ?assertEqual("01:00:00.590", vice_utils:to_full_hms(3600.59)),
        ?assertEqual("10:00:00", vice_utils:to_hms(36000.59)),
        ?assertEqual("10:00:00.590", vice_utils:to_full_hms(36000.59))
    end
   ]}.
