% @hidden
-module(vice_command).

-export([exec/4, get_data/2]).

exec(Command, Options, Module, Ref) ->
  error_logger:warning_msg("RUN ~ts", [Command]),
  bucos:run(
    Command,
    [
     {timeout, infinity},
     stdout_on_strerr,
     {return, list, all},
     {on_data, {fun ?MODULE:get_data/2, {Module, Ref, {undefined, undefined, 0.0}}}}
     | Options
    ]
   ).

get_data(Bytes, {Module, Ref, Sofar}) ->
  {_, _, Percent} = NSofar = Module:progress(Bytes, Sofar),
  vice_prv_status:value(Ref, Percent),
  error_logger:info_msg("[~p] convert ~p%", [Ref, vice_prv_status:value(Ref)]),
  {Module, Ref, NSofar}.
