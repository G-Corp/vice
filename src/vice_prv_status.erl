% @hidden
-module(vice_prv_status).

-export([
         insert/2
         , insert/3
         , delete/1
         , value/2
         , value/1
         , pid/1
        ]).

-define(TABLE, vice_status).

exist() ->
  case ets:info(?TABLE) of
    undefined ->
      ets:new(?TABLE, [public, named_table]);
    _ ->
      ok
  end.

insert(Ref, Pid) ->
  insert(Ref, Pid, 0.0).

insert(Ref, Pid, Value) ->
  exist(),
  ets:insert(?TABLE, {Ref, Pid, Value}),
  ok.

delete(Ref) ->
  exist(),
  ets:delete(?TABLE, Ref),
  ok.

value(Ref, Value) ->
  exist(),
  case ets:update_element(?TABLE, Ref, {3, Value}) of
    true -> ok;
    false -> {error, not_found}
  end.

value(Ref) ->
  exist(),
  case ets:lookup(?TABLE, Ref) of
    [{Ref, _Pid, Value}] ->
      Value;
    _ ->
      undefined
  end.

pid(Ref) ->
  exist(),
  case ets:lookup(?TABLE, Ref) of
    [{Ref, Pid, _Value}] ->
      Pid;
    _ ->
      undefined
  end.

