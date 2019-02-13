-module(vice_thumbnails_tests).
-include_lib("eunit/include/eunit.hrl").

hms_test_() ->
  {setup,
   fun() ->
     vice:start(),
     ok
   end,
   fun(_) ->
     application:stop(vice),
     application:stop(poolgirl),
     ok
   end,
   [
    fun() ->
        ?assertEqual(
          {error, 22.233},
          vice:thumbnails("test/erlang.mp4", "webvtttest", [{out_path, "test/thumbnails"}, {assets_path, ""}, {every, 30}])
        ),
        ?assertMatch(
          {async, _},
          vice:thumbnails("test/erlang.mp4", "webvtttest", [{out_path, "test/thumbnails"}, {assets_path, ""}, {every, 1}])
        )
    end
   ]}.
