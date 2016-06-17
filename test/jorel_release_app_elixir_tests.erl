-module(jorel_release_app_elixir_tests).
-include_lib("eunit/include/eunit.hrl").

jorel_release_jorel_elixir_test_() ->
  {"Build release for Elixir app",
   {setup,
    fun() ->
        bucfile:make_dir(".tests")
    end,
    fun(_) ->
        bucfile:remove_recursive(".tests")
    end,
    [
     {timeout, 200, 
      fun() ->
          ?assertEqual(ok, bucfile:make_dir(".tests/0.0.1")),
          ?assertEqual(ok, bucfile:copy("test_apps/0.0.1/elixir_test", ".tests/0.0.1", [recursive])),
          ?assertEqual(ok, bucfile:make_dir(".tests/0.0.1/elixir_test/.jorel")),
          ?assertEqual(ok, bucfile:copy("_build/default/bin/jorel", ".tests/0.0.1/elixir_test/.jorel/jorel"))
      end}
     , {timeout, 200, 
        fun() ->
            ?assertMatch({ok, _}, 
                         sh:sh("mix compile",
                               [return_on_error, {cd, ".tests/0.0.1/elixir_test"}]))
        end}
     , {timeout, 200, 
        fun() ->
            ?assertMatch({ok, _}, 
                         sh:sh(".jorel/jorel gen_config -v 0.0.1",
                               [return_on_error, {cd, ".tests/0.0.1/elixir_test"}]))
        end}
     , {timeout, 200, 
        fun() ->
            ?assertMatch({ok, _}, 
                         sh:sh(".jorel/jorel release",
                               [return_on_error, {cd, ".tests/0.0.1/elixir_test"}]))
        end}
     , {timeout, 200, 
        fun() ->
            ?assertMatch({error, {1, "Node 'elixir_test@127.0.0.1' not responding to pings.\n"}},
                         sh:sh("_jorel/elixir_test/bin/elixir_test ping",
                               [return_on_error, {cd, ".tests/0.0.1/elixir_test"}]))
        end}
     , {timeout, 200, 
        fun() ->
            ?assertMatch({ok, []},
                         sh:sh("_jorel/elixir_test/bin/elixir_test start",
                               [return_on_error, {cd, ".tests/0.0.1/elixir_test"}])),
            timer:sleep(1000)
        end}
     , {timeout, 200, 
        fun() ->
            ?assertMatch({ok, "pong\n"},
                         sh:sh("_jorel/elixir_test/bin/elixir_test ping",
                               [return_on_error, {cd, ".tests/0.0.1/elixir_test"}]))
        end}
     , {timeout, 200, 
        fun() ->
            ?assertMatch({ok, []},
                         sh:sh("_jorel/elixir_test/bin/elixir_test stop",
                               [return_on_error, {cd, ".tests/0.0.1/elixir_test"}]))
        end}
     , {timeout, 200, 
        fun() ->
            ?assertMatch({error, {1, "Node 'elixir_test@127.0.0.1' not responding to pings.\n"}},
                         sh:sh("_jorel/elixir_test/bin/elixir_test ping",
                               [return_on_error, {cd, ".tests/0.0.1/elixir_test"}]))
        end}
    ]}}.
