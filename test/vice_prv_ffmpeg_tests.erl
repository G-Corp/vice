-module(vice_prv_ffmpeg_tests).
-include_lib("eunit/include/eunit.hrl").

-record(state, {
          ffprobe,
          ffmpeg,
          openssl
         }).

vice_prv_ffmpeg_test_() ->
  {setup,
   fun() ->
       ok
   end,
   fun(_) ->
       ok
   end,
   [
    fun() ->
        State = #state{ffmpeg = "ffmpeg"},
        ?assertEqual(
           {ok, "ffmpeg -y -i \"in.mp4\" \"out.mp4\""},
           vice_prv_ffmpeg:command(State, "in.mp4", "out.mp4", [], false)),
        ?assertEqual(
           {ok, "ffmpeg -canvas_size 2 -y -i \"in.mp4\" \"out.mp4\""},
           vice_prv_ffmpeg:command(State, "in.mp4", "out.mp4", [{canvas_size, 2}], false)),
        ?assertEqual(
           {ok, "ffmpeg -canvas_size 2 -y -ss 0 -i \"in.mp4\" \"out.mp4\""},
           vice_prv_ffmpeg:command(State, "in.mp4", "out.mp4",
                                   [{canvas_size, 2},
                                    {input_position, 0}],
                                   false)),
        ?assertEqual(
           {ok, "ffmpeg -canvas_size 2 -y -t 10 -ss 0 -i \"in.mp4\" \"out.mp4\""},
           vice_prv_ffmpeg:command(State, "in.mp4", "out.mp4",
                                   [{canvas_size, 2},
                                    {input_duration, 10},
                                    {input_position, 0}],
                                   false)),
        ?assertEqual(
           {ok, "ffmpeg -canvas_size 2 -y -t 10 -ss 0 -i \"in.mp4\" -t 1 \"out.mp4\""},
           vice_prv_ffmpeg:command(State, "in.mp4", "out.mp4",
                                   [{canvas_size, 2},
                                    {input_duration, 10},
                                    {input_position, 0},
                                    {output_duration, 1}],
                                   false))
    end
   ]}.
