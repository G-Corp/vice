-module(vice_prv_subs_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("bucs/include/bucassert.hrl").

vice_prv_subs_tests_test_() ->
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
           {ok, 1, 2, [{digit, {1, 1, 2}, "1"}]},
           vice_prv_subs:tokenize("1")),
        ?assertEqual(
           {ok, 1, 6, [{string, {1, 1, 6}, "Hello"}]},
           vice_prv_subs:tokenize("Hello"))
    end,
    fun() ->
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("\n\n\n\n"),
           Tokens,
           fun(T) ->
               ?assertMatch(
                  {error, {{4, 1, 2}, vice_prv_subs_parser, _}},
                  vice_prv_subs_parser:parse(T))
           end)
    end,
    fun() ->
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("\n\n\n\n\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n\n\n\n"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("\n\n\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n\n\n\n\n\n"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"}]}},
                  vice_prv_subs_parser:parse(T))
           end)
    end,
    fun() ->
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("\n\n\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!\n\n\n\n\n"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("\n\n\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!\n\n\n\n\n"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}]}},
                  vice_prv_subs_parser:parse(T))
           end)
    end,
    fun() ->
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("1\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n2\n00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{identifier => "1",
                                    duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{identifier => "2",
                                    duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}]}},
                  vice_prv_subs_parser:parse(T))
           end)
    end,
    fun() ->
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("1\n00:00:00.000 --> 00:00:01.000 position:10%,line-left align:left size:35%\nHello World\nThis is good!\n\n\n\n" ++
                                  "2\n00:00:01.000 --> 00:00:02.000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{identifier => "1",
                                    duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  settings => [{"position", "10%,line-left"}, {"align", "left"}, {"size", "35%"}]},
                                    text => "Hello World\nThis is good!"},
                                  #{identifier => "2",
                                    duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("1\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n" ++
                                  "2\n00:00:01,000 --> 00:00:02,000 position:10%,line-left align:left size:35%\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{identifier => "1",
                                    duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{identifier => "2",
                                    duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"},
                                                  settings => [{"position", "10%,line-left"}, {"align", "left"}, {"size", "35%"}]},
                                    text => "Hola Mundo!"}]}},
                  vice_prv_subs_parser:parse(T))
           end)
    end,
    fun() ->
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00.000 --> 00:00:01.000 position:10%,line-left align:left size:35%\nHello World\nThis is good!\n\n\n\n" ++
                                  "00:00:01.000 --> 00:00:02.000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  settings => [{"position", "10%,line-left"}, {"align", "left"}, {"size", "35%"}]},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n" ++
                                  "00:00:01,000 --> 00:00:02,000 position:10%,line-left align:left size:35%\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"},
                                                  settings => [{"position", "10%,line-left"}, {"align", "left"}, {"size", "35%"}]},
                                    text => "Hola Mundo!"}]}},
                  vice_prv_subs_parser:parse(T))
           end)
    end,
    fun() ->
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\nNOTE hello\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!",
                                    note => "hello"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\nNOTE hello\n00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!",
                                    note => "hello"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\nNOTE\nhello\nworld\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!",
                                    note => "hello\nworld"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\nNOTE\nhello\nworld\n00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!",
                                    note => "hello\nworld"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!\n\nNOTE hola"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!",
                                    note => "hola"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!\nNOTE hola"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!",
                                    note => "hola"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!\n\nNOTE\nhola\nmundo"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!",
                                    note => "hola\nmundo"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!\nNOTE\nhola\nmundo"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!",
                                    note => "hola\nmundo"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!\n\nNOTE\nhola\nmundo\n\n\n"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!",
                                    note => "hola\nmundo"}]}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!\nNOTE\nhola\nmundo\n\n\n"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!",
                                    note => "hola\nmundo"}]}},
                  vice_prv_subs_parser:parse(T))
           end)
    end,
    fun() ->
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("\n\n\n\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}]}},
                  vice_prv_subs_parser:parse(T))
           end)
    end,
    fun() ->
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("WEBVTT\n\n\n\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}],
                         codec_private => #{webvtt => ""}}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("WEBVTT\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}],
                         codec_private => #{webvtt => ""}}},
                  vice_prv_subs_parser:parse(T))
           end)
    end,
    fun() ->
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("WEBVTT by greg, June 2017\n\n\n\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}],
                         codec_private => #{webvtt => "by greg, June 2017"}}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("WEBVTT by greg, June 2017\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}],
                         codec_private => #{webvtt => "by greg, June 2017"}}},
                  vice_prv_subs_parser:parse(T))
           end)
    end,
    fun() ->
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("WEBVTT by greg, June 2017\n\n\n\n" ++
                                  "NOTE this is a WEBVTT !\n\n\n\n" ++
                                  "00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n" ++
                                  "00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}],
                         codec_private => #{webvtt => "by greg, June 2017",
                                            notes => ["this is a WEBVTT !"]}}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("WEBVTT by greg, June 2017\n" ++
                                  "NOTE this is a WEBVTT !\n" ++
                                  "00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n" ++
                                  "00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}],
                         codec_private => #{webvtt => "by greg, June 2017",
                                            notes => ["this is a WEBVTT !"]}}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("WEBVTT by greg, June 2017\n\n" ++
                                  "NOTE\nthis is\na WEBVTT !\n\n" ++
                                  "00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n" ++
                                  "00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}],
                         codec_private => #{webvtt => "by greg, June 2017",
                                            notes => ["this is\na WEBVTT !"]}}},
                  vice_prv_subs_parser:parse(T))
           end)
    end,
    fun() ->
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("WEBVTT by greg, June 2017\n\n" ++
                                  "STYLE\n::cue(b) {\ncolor: blue;\n}\n\n\n" ++
                                  "00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n" ++
                                  "00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}],
                         codec_private => #{webvtt => "by greg, June 2017",
                                            styles => ["::cue(b) {\ncolor: blue;\n}"]}}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("WEBVTT by greg, June 2017\n\n" ++
                                  "STYLE\n::cue(b) {\ncolor: blue;\n}\n::cue {\ncolor: white;\n}\n\n" ++
                                  "00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n" ++
                                  "00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}],
                         codec_private => #{webvtt => "by greg, June 2017",
                                            styles => ["::cue(b) {\ncolor: blue;\n}\n::cue {\ncolor: white;\n}"]}}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("WEBVTT by greg, June 2017\n\n" ++
                                  "STYLE\n::cue(b) {\ncolor: blue;\n}\n\n" ++
                                  "STYLE\n::cue {\ncolor: white;\n}\n\n" ++
                                  "00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n" ++
                                  "00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}],
                         codec_private => #{webvtt => "by greg, June 2017",
                                            styles => ["::cue(b) {\ncolor: blue;\n}", "::cue {\ncolor: white;\n}"]}}},
                  vice_prv_subs_parser:parse(T))
           end)
    end,
    fun() ->
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("WEBVTT by greg, June 2017\n" ++
                                  "REGION\nid:john\nwidth:50%\nlines:3\n\n\n" ++
                                  "00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n" ++
                                  "00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}],
                         codec_private => #{webvtt => "by greg, June 2017",
                                            regions => ["id:john\nwidth:50%\nlines:3"]}}},
                  vice_prv_subs_parser:parse(T))
           end)
    end,
    fun() ->
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("WEBVTT by greg, June 2017\n" ++
                                  "NOTE note1\n" ++
                                  "REGION\nid:john\nwidth:50%\nlines:3\n" ++
                                  "STYLE\n::cue {\ncolor: green;\n}\n" ++
                                  "NOTE\nsecond\nnote\n" ++
                                  "00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n" ++
                                  "00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!"}],
                         codec_private => #{webvtt => "by greg, June 2017",
                                            notes => ["note1", "second\nnote"],
                                            styles => ["::cue {\ncolor: green;\n}"],
                                            regions => ["id:john\nwidth:50%\nlines:3"]}}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("WEBVTT by greg, June 2017\n" ++
                                  "NOTE note1\n" ++
                                  "REGION\nid:john\nwidth:50%\nlines:3\n" ++
                                  "STYLE\n::cue {\ncolor: green;\n}\nNOTE\nsecond\nnote\n" ++
                                  "1\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n" ++
                                  "2\n00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    text => "Hello World\nThis is good!",
                                    identifier => "1"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    text => "Hola Mundo!",
                                    identifier => "2"}],
                         codec_private => #{webvtt => "by greg, June 2017",
                                            notes => ["note1", "second\nnote"],
                                            styles => ["::cue {\ncolor: green;\n}"],
                                            regions => ["id:john\nwidth:50%\nlines:3"]}}},
                  vice_prv_subs_parser:parse(T))
           end),
        ?assertContinueIfMatch(
           {ok, _, _, Tokens},
           vice_prv_subs:tokenize("WEBVTT by greg, June 2017\n\n\n\n" ++
                                  "NOTE note1\n\n\n\n" ++
                                  "REGION\nid:john\nwidth:50%\nlines:3\n\n\n\n" ++
                                  "STYLE\n::cue {\ncolor: green;\n}\n\n\n\n" ++
                                  "NOTE\nsecond\nnote\n\n\n\n" ++
                                  "1\n00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\nNOTE this is the one\n\n\n" ++
                                  "2\n00:00:01,000 --> 00:00:02,000\nHola Mundo!\n\n\nNOTE\nThis is\nthe two\n\n\n\n"),
           Tokens,
           fun(T) ->
               ?assertEqual(
                  {ok, #{cues => [#{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "00"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "01"}},
                                    note => "this is the one",
                                    text => "Hello World\nThis is good!",
                                    identifier => "1"},
                                  #{duration => #{from => #{ex => "000", hh => "00", mm => "00", ss => "01"},
                                                  to => #{ex => "000", hh => "00", mm => "00", ss => "02"}},
                                    note => "This is\nthe two",
                                    text => "Hola Mundo!",
                                    identifier => "2"}],
                         codec_private => #{webvtt => "by greg, June 2017",
                                            notes => ["note1", "second\nnote"],
                                            styles => ["::cue {\ncolor: green;\n}"],
                                            regions => ["id:john\nwidth:50%\nlines:3"]}}},
                  vice_prv_subs_parser:parse(T))
           end)
    end
   ]}.
