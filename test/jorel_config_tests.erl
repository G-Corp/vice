-module(jorel_config_tests).
-include_lib("eunit/include/eunit.hrl").

jorel_config_test_() ->
  {"Test config", 
   {setup,
    fun() ->
        jorel_config:to_state([{config, "test/jorel-1.config"}], [])
    end,
    fun(_) -> ok end,
    fun(S) ->
        {with, S, 
         [fun(State) ->
              ?assertMatch({outdir, "_jorel/my_app"},
                           jorel_config:get(State, outdir))
          end,
          fun(State) ->
              ?assertMatch({relname, my_app},
                           jorel_config:get(State, relname))
          end,
          fun(State) ->
              ?assertMatch({relvsn, "1.0.0"},
                           jorel_config:get(State, relvsn))
          end,
          fun(State) ->
              ?assertMatch({binfile, "_jorel/my_app/bin/my_app"},
                           jorel_config:get(State, binfile))
          end,
          fun(State) ->
              ?assertMatch({erts, local},
                           jorel_config:get(State, erts))
          end
         ]}
    end}}.

