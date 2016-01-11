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
                                          [return_on_error, {cd, ".tests/jorel_sample"}])),
                       ?assertMatch({error, {1, "Node 'jorel_sample@127.0.0.1' not responding to pings.\n"}},
                                    sh:sh("_jorel/jorel_sample/bin/jorel_sample ping",
                                          [return_on_error, {cd, ".tests/jorel_sample"}])),
                       ?assertMatch({ok, []},
                                    sh:sh("_jorel/jorel_sample/bin/jorel_sample start",
                                          [return_on_error, {cd, ".tests/jorel_sample"}])),
                       timer:sleep(1000),
                       ?assertMatch({ok, "pong\n"},
                                    sh:sh("_jorel/jorel_sample/bin/jorel_sample ping",
                                          [return_on_error, {cd, ".tests/jorel_sample"}])),
                       ?assertMatch({ok, []},
                                    sh:sh("_jorel/jorel_sample/bin/jorel_sample stop",
                                          [return_on_error, {cd, ".tests/jorel_sample"}])),
                       ?assertMatch({error, {1, "Node 'jorel_sample@127.0.0.1' not responding to pings.\n"}},
                                    sh:sh("_jorel/jorel_sample/bin/jorel_sample ping",
                                          [return_on_error, {cd, ".tests/jorel_sample"}]))
                   end}
   ]}.
