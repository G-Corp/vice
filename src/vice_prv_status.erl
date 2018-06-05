% @hidden
-module(vice_prv_status).

-export([
         insert/2
         , insert/3
         , insert/4
         , delete/1
         , value/2
         , value/1
         , next/1
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

insert(Ref, Pid) when is_pid(Pid) ->
  insert(Ref, Pid, undefined).

insert(Ref, Pid, Port) when is_pid(Pid),
                            (is_port(Port) orelse Port == undefined) ->
  insert(Ref, Pid, Port, 0.0).

insert(Ref, Pid, Port, Value) when is_reference(Ref),
                                   is_pid(Pid),
                                   (is_port(Port) orelse Port == undefined) ->
  insert([{ref, Ref}], Pid, Port, Value);
insert(Options, Pid, Port, Value) when is_list(Options),
                                       is_pid(Pid),
                                       (is_port(Port) orelse Port == undefined) ->
  do_insert(
    proplists:get_value(ref, Options, erlang:make_ref()),
    Pid,
    Port,
    Value,
    proplists:get_value(stage, Options, 1),
    proplists:get_value(on, Options, 1)
   ).

do_insert(Ref, Pid, Port, Value, Stage, On) when is_reference(Ref),
                                            is_pid(Pid),
                                            (is_port(Port) orelse Port == undefined),
                                            is_integer(Stage),
                                            is_integer(On) ->
  exist(),
  ets:insert(?TABLE, {Ref, Pid, Port, Value, Stage, On}),
  Ref.

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
    [{Ref, _Pid, _Port, Value, Stage, On}] ->
      (((Stage - 1) / On) * 100) + (Value / On);
    _ ->
      undefined
  end.

next(Ref) when is_reference(Ref) ->
  exist(),
  case ets:lookup(?TABLE, Ref) of
    [{Ref, _Pid, _Port, _Value, Stage, On}] ->
      case Stage < On of
        true ->
          case ets:update_element(?TABLE, Ref, {5, Stage + 1}) of
            true -> Stage + 1;
            false -> error
          end;
        false -> On
      end;
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
    [{Ref, _Pid, Port, _Value, _Stage, _On}] ->
      Port;
    _ ->
      undefined
  end.

pid(Ref) when is_reference(Ref) ->
  exist(),
  case ets:lookup(?TABLE, Ref) of
    [{Ref, Pid, _Port, _Value, _Stage, _On}] ->
      Pid;
    _ ->
      undefined
  end.
