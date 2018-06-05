-module(vice_prv_status_tests).
-include_lib("eunit/include/eunit.hrl").

vice_prv_status_test_() ->
  {setup,
   fun() ->
       ok
   end,
   fun(_) ->
       ok
   end,
   [
    fun() ->
        Ref = make_ref(),

        ?assertEqual(undefined, vice_prv_status:value(Ref)),
        ?assertEqual(undefined, vice_prv_status:pid(Ref)),

        ?assertEqual(Ref, vice_prv_status:insert(Ref, self())),

        ?assertEqual(0.0, vice_prv_status:value(Ref)),
        ?assertEqual(self(), vice_prv_status:pid(Ref)),

        ?assertEqual(ok, vice_prv_status:value(Ref, 10.3)),
        ?assertEqual(10.3, vice_prv_status:value(Ref)),
        ?assertEqual(self(), vice_prv_status:pid(Ref)),

        ?assertEqual(ok, vice_prv_status:value(Ref, 76.45)),
        ?assertEqual(76.45, vice_prv_status:value(Ref)),
        ?assertEqual(self(), vice_prv_status:pid(Ref)),

        ?assertEqual(ok, vice_prv_status:delete(Ref)),
        ?assertEqual(undefined, vice_prv_status:value(Ref)),
        ?assertEqual(undefined, vice_prv_status:pid(Ref)),

        ?assertEqual({error, not_found}, vice_prv_status:value(Ref, 76.45)),

        ?assertEqual(ok, vice_prv_status:delete(Ref))
    end,
    fun() ->
        Ref = make_ref(),

        ?assertEqual(undefined, vice_prv_status:value(Ref)),
        ?assertEqual(undefined, vice_prv_status:pid(Ref)),

        ?assertEqual(Ref, vice_prv_status:insert([{ref, Ref}], self())),

        ?assertEqual(0.0, vice_prv_status:value(Ref)),
        ?assertEqual(self(), vice_prv_status:pid(Ref)),

        ?assertEqual(ok, vice_prv_status:value(Ref, 10.3)),
        ?assertEqual(10.3, vice_prv_status:value(Ref)),
        ?assertEqual(self(), vice_prv_status:pid(Ref)),

        ?assertEqual(ok, vice_prv_status:value(Ref, 76.45)),
        ?assertEqual(76.45, vice_prv_status:value(Ref)),
        ?assertEqual(self(), vice_prv_status:pid(Ref)),

        ?assertEqual(ok, vice_prv_status:delete(Ref)),
        ?assertEqual(undefined, vice_prv_status:value(Ref)),
        ?assertEqual(undefined, vice_prv_status:pid(Ref)),

        ?assertEqual({error, not_found}, vice_prv_status:value(Ref, 76.45)),

        ?assertEqual(ok, vice_prv_status:delete(Ref))
    end,
    fun() ->
        Ref = make_ref(),
        Options = [{ref, Ref}, {stage, 2}, {on, 3}],
        Value = fun(V) ->
                    ((1/3)*100) + (V/3)
                end,

        ?assertEqual(undefined, vice_prv_status:value(Ref)),
        ?assertEqual(undefined, vice_prv_status:pid(Ref)),

        ?assertEqual(Ref, vice_prv_status:insert(Options, self())),

        ?assertEqual(Value(0), vice_prv_status:value(Ref)),
        ?assertEqual(self(), vice_prv_status:pid(Ref)),

        ?assertEqual(ok, vice_prv_status:value(Ref, 10.3)),
        ?assertEqual(Value(10.3), vice_prv_status:value(Ref)),
        ?assertEqual(self(), vice_prv_status:pid(Ref)),

        ?assertEqual(ok, vice_prv_status:value(Ref, 76.45)),
        ?assertEqual(Value(76.45), vice_prv_status:value(Ref)),
        ?assertEqual(self(), vice_prv_status:pid(Ref)),

        ?assertEqual(ok, vice_prv_status:delete(Ref)),
        ?assertEqual(undefined, vice_prv_status:value(Ref)),
        ?assertEqual(undefined, vice_prv_status:pid(Ref)),

        ?assertEqual({error, not_found}, vice_prv_status:value(Ref, 76.45)),

        ?assertEqual(ok, vice_prv_status:delete(Ref))
    end
   ]}.
