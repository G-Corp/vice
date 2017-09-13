-module(vice_subtitles_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("bucs/include/bucassert.hrl").

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
    end,
    fun() ->
        ?assertContinueIfMatch(
           {ok, Subs},
           vice_subtitles:parse(
             "WEBVTT by greg, June 2017\n\n\n\n" ++
             "NOTE note1\n\n\n\n" ++
             "REGION\nid:john\nwidth:50%\nlines:3\n\n\n\n" ++
             "STYLE\n::cue {\ncolor: green;\n}\n\n\n\n" ++
             "NOTE\nsecond\nnote\n\n\n\n" ++
             "1\n00:00:00.000 --> 00:00:01.000\nHello World\nThis is good!\n\n\n\nNOTE this is the one\n\n\n" ++
             "2\n00:00:01.000 --> 00:00:02.000\nHola Mundo!\n\n\nNOTE\nThis is\nthe two\n\n\n\n"),
           Subs,
           fun(S) ->
               ?assertEqual(
                  {ok,
                   "1\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n" ++
                   "2\n00:00:01,000 --> 00:00:02,000\nHola Mundo!",
                   2000, 2.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, srt)),
               ?assertEqual(
                  {ok,
                   "1\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n" ++
                   "2\n00:00:01,000 --> 00:00:02,000\nHola Mundo!",
                   2000, 2.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, srt, #{from => 0, duration => 2})),
               ?assertEqual(
                  {ok,
                   "1\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n" ++
                   "2\n00:00:01,000 --> 00:00:02,000\nHola Mundo!",
                   2000, 2.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, srt, #{from => 0, duration => 1.1})),
               ?assertEqual(
                  {ok,
                   "1\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n" ++
                   "2\n00:00:01,000 --> 00:00:02,000\nHola Mundo!",
                   2000, 2.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, srt, #{from => 0, duration => 10})),
               ?assertEqual(
                  {ok,
                   "1\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n" ++
                   "2\n00:00:01,000 --> 00:00:02,000\nHola Mundo!",
                   2000, 2.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, srt, #{from => 0, to => "00:00:02"})),
               ?assertEqual(
                  {ok,
                   "1\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n" ++
                   "2\n00:00:01,000 --> 00:00:02,000\nHola Mundo!",
                   2000, 2.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, srt, #{from => 0, to => "00:01:00"})),
               ?assertEqual(
                  {ok,
                   "1\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n" ++
                   "2\n00:00:01,000 --> 00:00:02,000\nHola Mundo!",
                   2000, 2.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, srt, #{from => 0})),
               ?assertEqual(
                  {ok,
                   "1\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!",
                   1000, 1.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                       id => 0, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                     identifier => "1", note => "this is the one",
                     text => "Hello World\nThis is good!"}},
                  vice_subtitles:to_string(S, srt, #{from => 0, duration => 0.5})),
               ?assertEqual(
                  {ok,
                   "1\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!",
                   1000, 1.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                       id => 0, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                     identifier => "1", note => "this is the one",
                     text => "Hello World\nThis is good!"}},
                  vice_subtitles:to_string(S, srt, #{from => 0, duration => 1})),
               ?assertEqual(
                  {ok,
                   "1\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!",
                   1000, 1.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                       id => 0, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                     identifier => "1", note => "this is the one",
                     text => "Hello World\nThis is good!"}},
                  vice_subtitles:to_string(S, srt, #{from => 0, to => "00:00:01"})),
               ?assertEqual(
                  {ok,
                   "1\n00:00:01,000 --> 00:00:02,000\nHola Mundo!",
                   2000, 1.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, srt, #{from => 1000, to => "00:00:02"})),
               ?assertEqual(
                  {ok,
                   "1\n00:00:01,000 --> 00:00:02,000\nHola Mundo!",
                   2000, 1.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, srt, #{from => 1000, to => "00:00:09"})),
               ?assertEqual(
                  {ok,
                   "1\n00:00:01,000 --> 00:00:02,000\nHola Mundo!",
                   2000, 1.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, srt, #{from => 1000, duration => 1})),
               ?assertEqual(
                  {ok,
                   "1\n00:00:01,000 --> 00:00:02,000\nHola Mundo!",
                   2000, 1.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, srt, #{from => 1000, duration => 0.1})),
               ?assertEqual(
                  no_data,
                  vice_subtitles:to_string(S, srt, #{from => 10000, duration => 0.1}))
           end)
    end,
    fun() ->
        ?assertContinueIfMatch(
           {ok, Subs},
           vice_subtitles:parse(
             "WEBVTT by greg, June 2017\n\n\n\n" ++
             "NOTE note1\n\n\n\n" ++
             "REGION\nid:john\nwidth:50%\nlines:3\n\n\n\n" ++
             "STYLE\n::cue {\ncolor: green;\n}\n\n\n\n" ++
             "NOTE\nsecond\nnote\n\n\n\n" ++
             "1\n00:00:00.000 --> 00:00:01.000\nHello World\nThis is good!\n\n\n\nNOTE this is the one\n\n\n" ++
             "2\n00:00:01.000 --> 00:00:02.000\nHola Mundo!\n\n\nNOTE\nThis is\nthe two\n\n\n\n"),
           Subs,
           fun(S) ->
               ?assertEqual(
                  {ok,
                   "WEBVTT\n\n" ++
                   "1\n00:00:00.000 --> 00:00:01.000\nHello World\nThis is good!\n\n" ++
                   "2\n00:00:01.000 --> 00:00:02.000\nHola Mundo!",
                   2000, 2.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, webvtt)),
               ?assertEqual(
                  {ok,
                   "WEBVTT\n\n" ++
                   "1\n00:00:00.000 --> 00:00:01.000\nHello World\nThis is good!\n\n" ++
                   "2\n00:00:01.000 --> 00:00:02.000\nHola Mundo!",
                   2000, 2.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, webvtt, #{from => 0, duration => 2})),
               ?assertEqual(
                  {ok,
                   "WEBVTT\n\n" ++
                   "1\n00:00:00.000 --> 00:00:01.000\nHello World\nThis is good!\n\n" ++
                   "2\n00:00:01.000 --> 00:00:02.000\nHola Mundo!",
                   2000, 2.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, webvtt, #{from => 0, duration => 1.1})),
               ?assertEqual(
                  {ok,
                   "WEBVTT\n\n" ++
                   "1\n00:00:00.000 --> 00:00:01.000\nHello World\nThis is good!\n\n" ++
                   "2\n00:00:01.000 --> 00:00:02.000\nHola Mundo!",
                   2000, 2.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, webvtt, #{from => 0, duration => 10})),
               ?assertEqual(
                  {ok,
                   "WEBVTT\n\n" ++
                   "1\n00:00:00.000 --> 00:00:01.000\nHello World\nThis is good!\n\n" ++
                   "2\n00:00:01.000 --> 00:00:02.000\nHola Mundo!",
                   2000, 2.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, webvtt, #{from => 0, to => "00:00:02"})),
               ?assertEqual(
                  {ok,
                   "WEBVTT\n\n" ++
                   "1\n00:00:00.000 --> 00:00:01.000\nHello World\nThis is good!\n\n" ++
                   "2\n00:00:01.000 --> 00:00:02.000\nHola Mundo!",
                   2000, 2.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, webvtt, #{from => 0, to => "00:01:00"})),
               ?assertEqual(
                  {ok,
                   "WEBVTT\n\n" ++
                   "1\n00:00:00.000 --> 00:00:01.000\nHello World\nThis is good!\n\n" ++
                   "2\n00:00:01.000 --> 00:00:02.000\nHola Mundo!",
                   2000, 2.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, webvtt, #{from => 0})),
               ?assertEqual(
                  {ok,
                   "WEBVTT\n\n" ++
                   "1\n00:00:00.000 --> 00:00:01.000\nHello World\nThis is good!",
                   1000, 1.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                       id => 0, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                     identifier => "1", note => "this is the one",
                     text => "Hello World\nThis is good!"}},
                  vice_subtitles:to_string(S, webvtt, #{from => 0, duration => 0.5})),
               ?assertEqual(
                  {ok,
                   "WEBVTT\n\n" ++
                   "1\n00:00:00.000 --> 00:00:01.000\nHello World\nThis is good!",
                   1000, 1.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                       id => 0, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                     identifier => "1", note => "this is the one",
                     text => "Hello World\nThis is good!"}},
                  vice_subtitles:to_string(S, webvtt, #{from => 0, duration => 1})),
               ?assertEqual(
                  {ok,
                   "WEBVTT\n\n" ++
                   "1\n00:00:00.000 --> 00:00:01.000\nHello World\nThis is good!",
                   1000, 1.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                       id => 0, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                     identifier => "1", note => "this is the one",
                     text => "Hello World\nThis is good!"}},
                  vice_subtitles:to_string(S, webvtt, #{from => 0, to => "00:00:01"})),
               ?assertEqual(
                  {ok,
                   "WEBVTT\n\n" ++
                   "1\n00:00:01.000 --> 00:00:02.000\nHola Mundo!",
                   2000, 1.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, webvtt, #{from => 1000, to => "00:00:02"})),
               ?assertEqual(
                  {ok,
                   "WEBVTT\n\n" ++
                   "1\n00:00:01.000 --> 00:00:02.000\nHola Mundo!",
                   2000, 1.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, webvtt, #{from => 1000, to => "00:00:09"})),
               ?assertEqual(
                  {ok,
                   "WEBVTT\n\n" ++
                   "1\n00:00:01.000 --> 00:00:02.000\nHola Mundo!",
                   2000, 1.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, webvtt, #{from => 1000, duration => 1})),
               ?assertEqual(
                  {ok,
                   "WEBVTT\n\n" ++
                   "1\n00:00:01.000 --> 00:00:02.000\nHola Mundo!",
                   2000, 1.0,
                   #{duration =>
                     #{duration => 1.0,
                       from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                       id => 1000, length => 1000,
                       to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                     identifier => "2", note => "This is\nthe two", text => "Hola Mundo!"}},
                  vice_subtitles:to_string(S, webvtt, #{from => 1000, duration => 0.1})),
               ?assertEqual(
                  no_data,
                  vice_subtitles:to_string(S, webvtt, #{from => 10000, duration => 0.1}))
           end)
    end
   ]}.
