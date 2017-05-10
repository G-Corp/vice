% @hidden
-module(vice_prv_status).

-export([
         insert/2
         , insert/3
         , insert/4
         , delete/1
         , value/2
         , value/1
         , port/2
         , port/1
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

insert(Ref, Pid) when is_reference(Ref), is_pid(Pid) ->
  insert(Ref, Pid, undefined).

insert(Ref, Pid, Port) when is_reference(Ref), is_pid(Pid), (is_port(Port) orelse Port == undefined) ->
  insert(Ref, Pid, Port, 0.0).

insert(Ref, Pid, Port, Value) when is_reference(Ref), is_pid(Pid), (is_port(Port) orelse Port == undefined) ->
  exist(),
  ets:insert(?TABLE, {Ref, Pid, Port, Value}),
  ok.

delete(Ref) when is_reference(Ref) ->
  exist(),
  ets:delete(?TABLE, Ref),
  ok.

value(Ref, Value) when is_reference(Ref) ->
  exist(),
  case ets:update_element(?TABLE, Ref, {4, Value}) of
    true -> ok;
    false -> {error, not_found}
  end.

value(Ref) when is_reference(Ref) ->
  exist(),
  case ets:lookup(?TABLE, Ref) of
    [{Ref, _Pid, _Port, Value}] ->
      Value;
    _ ->
      undefined
  end.

port(Ref, Port) when is_reference(Ref), is_port(Port) ->
  exist(),
  case ets:update_element(?TABLE, Ref, {3, Port}) of
    true -> ok;
    false -> {error, not_found}
  end.

port(Ref) when is_reference(Ref) ->
  exist(),
  case ets:lookup(?TABLE, Ref) of
    [{Ref, _Pid, Port, _Value}] ->
      Port;
    _ ->
      undefined
  end.

pid(Ref) when is_reference(Ref) ->
  exist(),
  case ets:lookup(?TABLE, Ref) of
    [{Ref, Pid, _Port, _Value}] ->
      Pid;
    _ ->
      undefined
  end.

