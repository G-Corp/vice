-module(vice_prv_subs_tests).
-include_lib("eunit/include/eunit.hrl").

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
    % fun() ->
    %     ?assertEqual(
    %        ok,
    %        vice_prv_subs:tokenize("WEBVTT\n\n00:00:00.000 --> 00:00:01.000\nHello World\nThis is good!\n\n00:00:01.000 --> 00:00:02.000\nHola Mundo !"))
    % end,
    fun() ->
        {ok, _, _, Tokens} = vice_prv_subs:tokenize("00:00:00.000 --> 00:00:01.000\nHello World\nThis is good!\n\n\n\n00:00:01.000 --> 00:00:02.000\nHola Mundo!"),
        ?debugFmt("~n~p~n", [Tokens]),
        ?assertEqual(
           {ok, [#{duration => #{from => #{ex => "000",hh => "00",mm => "00",ss => "00"},
                                 to => #{ex => "000",hh => "00",mm => "00",ss => "01"}},
                   text => "Hello World\nThis is good!\n"},
                 #{duration => #{from => #{ex => "000",hh => "00",mm => "00",ss => "01"},
                                 to => #{ex => "000",hh => "00",mm => "00",ss => "02"}},
                   text => "Hola Mundo!"}]},
           vice_prv_subs_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = vice_prv_subs:tokenize("00:00:00,000 --> 00:00:01,000\nHello World\nThis is good!\n\n\n\n00:00:01,000 --> 00:00:02,000\nHola Mundo!"),
        ?debugFmt("~n~p~n", [Tokens]),
        ?assertEqual(
           {ok, [#{duration => #{from => #{ex => "000",hh => "00",mm => "00",ss => "00"},
                                 to => #{ex => "000",hh => "00",mm => "00",ss => "01"}},
                   text => "Hello World\nThis is good!\n"},
                 #{duration => #{from => #{ex => "000",hh => "00",mm => "00",ss => "01"},
                                 to => #{ex => "000",hh => "00",mm => "00",ss => "02"}},
                   text => "Hola Mundo!"}]},
           vice_prv_subs_parser:parse(Tokens))
    end

   ]}.
