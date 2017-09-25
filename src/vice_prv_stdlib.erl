% @hidden
-module(vice_prv_stdlib).

-export([
         ceil/1
         , chomp/1
        ]).

ceil(V) ->
  try
    erlang:ceil(V)
  catch
    _:_ ->
      T = erlang:trunc(V) * 1.0,
      if
        V - T > 0 -> T + 1;
        true -> T
      end
  end.

chomp(S) when is_list(S) ->
  try
    string:chomp(S)
  catch
    _:_ ->
      re:replace(S, "(^\\s+)|([\\s\\t\\r\\n]+$)", "", [global, {return, list}])
  end.
