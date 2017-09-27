-module(vice_prv_options_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/vice_ffmpeg.hrl").

vice_prv_options_test_() ->
  {setup,
   fun() ->
       ok
   end,
   fun(_) ->
       ok
   end,
   [
    fun() ->
        ?assert(vice_prv_options:is_true(true)),
        ?assertNot(vice_prv_options:is_true(false)),
        ?assertNot(vice_prv_options:is_true(atom)),
        ?assertNot(vice_prv_options:is_true("string")),
        ?assertNot(vice_prv_options:is_true(<<"binary">>)),
        ?assertNot(vice_prv_options:is_true(123))
    end,
    fun() ->
        ?assert(vice_prv_options:is_list_and_list([[], []])),
        ?assert(vice_prv_options:is_list_and_list([[1], [2]])),
        ?assertNot(vice_prv_options:is_list_and_list([atom, [2]])),
        ?assertNot(vice_prv_options:is_list_and_list([<<"binary">>, [2]])),
        ?assertNot(vice_prv_options:is_list_and_list([1, [2]])),
        ?assertNot(vice_prv_options:is_list_and_list([[1], atom])),
        ?assertNot(vice_prv_options:is_list_and_list([[1], <<"binary">>])),
        ?assertNot(vice_prv_options:is_list_and_list([[1], 2]))
    end,
    fun() ->
        ?assert(vice_prv_options:is_list_and_integer([[], 1])),
        ?assert(vice_prv_options:is_list_and_integer([[1], 2])),
        ?assertNot(vice_prv_options:is_list_and_integer([atom, 2])),
        ?assertNot(vice_prv_options:is_list_and_integer([<<"binary">>, 2])),
        ?assertNot(vice_prv_options:is_list_and_integer([1, 2])),
        ?assertNot(vice_prv_options:is_list_and_integer([[1], atom])),
        ?assertNot(vice_prv_options:is_list_and_integer([[1], <<"binary">>])),
        ?assertNot(vice_prv_options:is_list_and_integer([[1], [2]]))
    end,
    fun() ->
        ?assert(vice_prv_options:is_list_and_float([[], 1.0])),
        ?assert(vice_prv_options:is_list_and_float([[1], 2.0])),
        ?assertNot(vice_prv_options:is_list_and_float([atom, 2.0])),
        ?assertNot(vice_prv_options:is_list_and_float([<<"binary">>, 2.0])),
        ?assertNot(vice_prv_options:is_list_and_float([1, 2.0])),
        ?assertNot(vice_prv_options:is_list_and_float([[1], atom])),
        ?assertNot(vice_prv_options:is_list_and_float([[1], <<"binary">>])),
        ?assertNot(vice_prv_options:is_list_and_float([[1], [2]])),
        ?assertNot(vice_prv_options:is_list_and_float([[1], 2]))
    end,
    fun() ->
        ?assert(vice_prv_options:is_list_and_true([[], true])),
        ?assert(vice_prv_options:is_list_and_true([[1], true])),
        ?assertNot(vice_prv_options:is_list_and_true([atom, true])),
        ?assertNot(vice_prv_options:is_list_and_true([<<"binary">>, true])),
        ?assertNot(vice_prv_options:is_list_and_true([1, true])),
        ?assertNot(vice_prv_options:is_list_and_true([[1], atom])),
        ?assertNot(vice_prv_options:is_list_and_true([[1], <<"binary">>])),
        ?assertNot(vice_prv_options:is_list_and_true([[1], [2]])),
        ?assertNot(vice_prv_options:is_list_and_true([[1], false]))
    end,
    fun() ->
        ?assert(vice_prv_options:is_integer_and_list([1, []])),
        ?assert(vice_prv_options:is_integer_and_list([1, [2]])),
        ?assertNot(vice_prv_options:is_integer_and_list([atom, [2]])),
        ?assertNot(vice_prv_options:is_integer_and_list([<<"binary">>, [2]])),
        ?assertNot(vice_prv_options:is_integer_and_list([[1], [2]])),
        ?assertNot(vice_prv_options:is_integer_and_list([1, atom])),
        ?assertNot(vice_prv_options:is_integer_and_list([1, <<"binary">>])),
        ?assertNot(vice_prv_options:is_integer_and_list([1, 2]))
    end,
    fun() ->
        ?assert(vice_prv_options:is_integer_and_integer([1, 2])),
        ?assertNot(vice_prv_options:is_integer_and_integer([atom, 2])),
        ?assertNot(vice_prv_options:is_integer_and_integer([<<"binary">>, 2])),
        ?assertNot(vice_prv_options:is_integer_and_integer(["string", 2])),
        ?assertNot(vice_prv_options:is_integer_and_integer([1, atom])),
        ?assertNot(vice_prv_options:is_integer_and_integer([1, <<"binary">>])),
        ?assertNot(vice_prv_options:is_integer_and_integer([1, "string"]))
    end,
    fun() ->
        ?assert(vice_prv_options:is_integer_and_float([1, 2.0])),
        ?assertNot(vice_prv_options:is_integer_and_float([atom, 2.0])),
        ?assertNot(vice_prv_options:is_integer_and_float([<<"binary">>, 2.0])),
        ?assertNot(vice_prv_options:is_integer_and_float(["string", 2.0])),
        ?assertNot(vice_prv_options:is_integer_and_float([1.0, 2.0])),
        ?assertNot(vice_prv_options:is_integer_and_float([1, atom])),
        ?assertNot(vice_prv_options:is_integer_and_float([1, <<"binary">>])),
        ?assertNot(vice_prv_options:is_integer_and_float([1, [2]])),
        ?assertNot(vice_prv_options:is_integer_and_float([1, false])),
        ?assertNot(vice_prv_options:is_integer_and_float([1, 1]))
    end,
    fun() ->
        ?assertEqual(" hello", vice_prv_options:to_arg("hello")),
        ?assertEqual(" 1", vice_prv_options:to_arg(1)),
        ?assertEqual(" 1.0", vice_prv_options:to_arg(1.0))
    end,
    fun() ->
        ?assertEqual("", vice_prv_options:to_nothing("hello")),
        ?assertEqual("", vice_prv_options:to_nothing(1)),
        ?assertEqual("", vice_prv_options:to_nothing(1.0))
    end,
    fun() ->
        ?assertEqual(":1 hello", vice_prv_options:to_dotargs([1, "hello"])),
        ?assertEqual(":1 2.0", vice_prv_options:to_dotargs([1, 2.0])),
        ?assertEqual(":hello world", vice_prv_options:to_dotargs(["hello", "world"])),
        ?assertEqual(":hello 1", vice_prv_options:to_dotargs(["hello", 1])),
        ?assertEqual(":hello 2.0", vice_prv_options:to_dotargs(["hello", 2.0])),
        ?assertEqual(":hello", vice_prv_options:to_dotargs(["hello", true]))
    end,
    fun() ->
        ?assertEqual(" hello=\"world\"", vice_prv_options:to_kvarg(["hello", "world"]))
    end,
    fun() ->
        ?assertEqual(
           [],
           vice_prv_options:options(?OPTIONS, [])
          )
        , ?assertEqual(
             [{global, " -y"}],
             vice_prv_options:options(?OPTIONS, [{yes, true}])
            )
        , ?assertEqual(
             [{global, " -fix_sub_duration"}],
             vice_prv_options:options(?OPTIONS, [{fix_sub_duration, true}])
            )
        , ?assertEqual(
             [{global, " -canvas_size 1"}],
             vice_prv_options:options(?OPTIONS, [{canvas_size, 1}])
            )
        , ?assertEqual(
             [{global, " -filter_complex acrossfade=d=10:c1=exp:c2=exp"}],
             vice_prv_options:options(?OPTIONS, [{filter_complex, "acrossfade=d=10:c1=exp:c2=exp"}])
            )
        , ?assertEqual(
             [{global, " -filter_complex_script script"}],
             vice_prv_options:options(?OPTIONS, [{filter_complex_script, "script"}])
            )
        , ?assertEqual(
             [{input, " -ss 2"}],
             vice_prv_options:options(?OPTIONS, [{input_position, 2}])
            )
        , ?assertEqual(
             [{input, " -ss 01:02:03"}],
             vice_prv_options:options(?OPTIONS, [{input_position, "01:02:03"}])
            )
        , ?assertEqual(
             [{input, " -sseof 2"}],
             vice_prv_options:options(?OPTIONS, [{input_eof_position, 2}])
            )
        , ?assertEqual(
             [{input, " -sseof 01:02:03"}],
             vice_prv_options:options(?OPTIONS, [{input_eof_position, "01:02:03"}])
            )
        , ?assertEqual(
             [{input, " -t 2"}],
             vice_prv_options:options(?OPTIONS, [{input_duration, 2}])
            )
        , ?assertEqual(
             [{input, " -t 01:02:03"}],
             vice_prv_options:options(?OPTIONS, [{input_duration, "01:02:03"}])
            )
    end
   ]}.
