% @hidden
-module(jorel_cmd).
-export([run/1, run/2]).

run(Cmd) ->
  run(Cmd, 5000).

run(Cmd, Timeout) ->
  Port = erlang:open_port({spawn, Cmd}, [exit_status]),
  loop(Port, [], Timeout).

loop(Port, Data, Timeout) ->
  receive
    {Port, {data, NewData}} -> loop(Port, Data ++ NewData, Timeout);
    {Port, {exit_status, 0}} -> Data;
    {Port, {exit_status, S}} -> throw({commandfailed, S})
  after Timeout ->
          throw(timeout)
  end.

