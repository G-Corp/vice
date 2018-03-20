% @hidden
-module(vice_command).
-compile([{parse_transform, lager_transform}]).

-export([exec/3, get_data/2]).

exec(Command, Module, Ref) ->
  lager:debug("COMMAND : ~p", [Command]),
  bucos:run(
    Command,
    [
     {timeout, infinity},
     stdout_on_strerr,
     {return, list, all},
     {on_data, {fun ?MODULE:get_data/2, {Module, Ref, {undefined, undefined, 0.0}}}}
    ]
   ).

get_data(Bytes, {Module, Ref, Sofar}) ->
  {_, _, Percent} = NSofar = Module:progress(Bytes, Sofar),
  vice_prv_status:value(Ref, Percent),
  lager:debug("[~p] convert ~p%", [Ref, Percent]),
  {Module, Ref, NSofar}.

% exec(Command, Module, Ref) ->
%   Port = erlang:open_port({spawn, Command}, [stream, in, eof, hide, exit_status, stderr_to_stdout]),
%   vice_prv_status:port(Ref, Port),
%   get_data(Port, Module, Ref, {undefined, undefined, 0.0}).
%
% get_data(Port, Module, Ref, Sofar) ->
%   receive
%     {Port, {data, Bytes}} ->
%       {_, _, Percent} = NSofar = Module:progress(Bytes, Sofar),
%       vice_prv_status:value(Ref, Percent),
%       lager:debug("[~p] convert ~p%", [Ref, Percent]),
%       get_data(Port, Module, Ref, NSofar);
%     {Port, eof} ->
%       Port ! {self(), close},
%       receive
%         {Port, closed} ->
%           true
%       end,
%       receive
%         {'EXIT', Port, _} ->
%           ok
%       after 1 ->
%               ok
%       end,
%       receive
%         {Port, {exit_status, 0}} ->
%           {ok, 0, Sofar};
%         {Port, {exit_status, Code}} ->
%           {error, Code, Sofar}
%       end
%   end.
