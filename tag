#!/usr/bin/env escript

main(_) ->
  case file:consult("src/jorel.app.src") of
    {ok, [{application, jorel, Data}]} ->
      case lists:keyfind(vsn, 1, Data) of
        {vsn, Version} -> io:format("~s", [Version]);
        _ -> io:format("ERROR")
      end;
    _ -> io:format("ERROR")
  end.
