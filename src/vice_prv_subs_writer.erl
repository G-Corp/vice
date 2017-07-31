% @hidden
-module(vice_prv_subs_writer).

-export([to_string/3, to_file/4]).

-define(DEFAULT_SEGMENTS, #{
          segment_time => 2,
          segment_filename => "subtitle_%d.vtt",
          from => 0,
          to => undefined,
          duration => undefined
         }).

to_string(_Subs, _Type, _Options) ->
  todo.

to_file(_Subs, _Type, _File, _Options) ->
  todo.
