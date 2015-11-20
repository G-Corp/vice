-module(jorel_elixir).

-export([exist/0,
         path/0,
         version/0]).

exist() ->
  (iex() =/= false) and filelib:is_file("mix.exs").

version() ->
  case iex() of
    false -> 
      {error, elixir_not_found};
    IEX -> 
      [Version|_] = lists:reverse(
                      string:tokens(
                        string:strip(
                          jorel_cmd:run(
                            IEX ++ " --version | tail -1"), 
                          right, 
                          10), 
                        " ")),
      {ok, Version}
  end.

path() ->
  case iex() of
    false -> 
      {error, elixir_not_found};
    IEX -> 
      {ok, string:strip(
             jorel_cmd:run(
               IEX ++ " --erl \"+A0\" -e '[elixir_lib_path, _] = String.split(\"#{:code.which(:elixir)}\", \"/elixir/ebin/elixir.beam\"); IO.puts elixir_lib_path |> Path.expand; System.halt' | tail -1"),
             right,
             10)}
  end.


iex() ->
  os:find_executable("iex").
