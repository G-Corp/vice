-module(vice_prv_sox_tests).
-include_lib("eunit/include/eunit.hrl").

-record(state, {
          sox,
          soxi
         }).

vice_prv_sox_test_() ->
  {setup,
   fun() ->
       ok
   end,
   fun(_) ->
       ok
   end,
   [
    fun() ->
        State = #state{sox = "sox"},
        ?assertEqual(
           {ok, "sox --clobber --show-progress \"input.wav\" \"output.ogg\""},
           vice_prv_sox:command(State, "input.wav", "output.ogg", [], false)),
        ?assertEqual(
           {ok, "sox --clobber --show-progress  \"left.wav\" \"right.wav\" \"stereo.ogg\"  remix 1,2v0.2 1v0.2,2"},
           vice_prv_sox:command(State, ["left.wav", "right.wav"], "stereo.ogg", [{effects, "remix 1,2v0.2 1v0.2,2"}], false)),
        ?assertEqual(
           {ok, "sox --clobber --show-progress  --volume 1.1 \"left.wav\" --volume 2.2 \"right.wav\" \"stereo.ogg\"  remix 1,2v0.2 1v0.2,2"},
           vice_prv_sox:command(
             State,
             ["left.wav", "right.wav"],
             "stereo.ogg",
             [{effects, "remix 1,2v0.2 1v0.2,2"},
              {volume, 1, 1.1},
              {volume, 2, 2.2}],
             false)),
        ?assertEqual(
           {ok, "sox --buffer 2 --clobber --show-progress  --volume 1.1 \"left.wav\" --volume 2.2 --endian big \"right.wav\" --channels 2 \"stereo.ogg\"  remix 1,2v0.2 1v0.2,2"},
           vice_prv_sox:command(
             State,
             ["left.wav", "right.wav"],
             "stereo.ogg",
             [{effects, "remix 1,2v0.2 1v0.2,2"},
              {buffer, 2},
              {volume, 1, 1.1},
              {volume, 2, 2.2},
              {input_endian, 2, big},
              {output_channels, 2}],
             false))
    end
   ]}.
