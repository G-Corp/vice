-module(jorel_release_app_tests).
-include_lib("eunit/include/eunit.hrl").

jorel_release_jorel_sample_test_() ->
  {setup,
   fun() ->
       _ = efile:make_dir(".tests"),
       efile:copy("test/test_apps/jorel_sample", ".tests", [recursive])
   end,
   fun(_) ->
       _ = efile:remove_recursive(".tests")
   end,
   [
    {timeout, 200, fun() ->
                       ?assertMatch({ok, _}, 
                                    sh:sh("make jorel.release",
                                          [return_on_error, {cd, ".tests/jorel_sample"}]))

                   end}
   ]}.
