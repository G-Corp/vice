-module(jorel_elixir).

-export([exist/0,
         path/0,
         version/0,
         config_to_sys_config/3]).

exist() ->
  (iex() =/= false) and (elixir() =/= false) and filelib:is_file("mix.exs").

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

config_to_sys_config(Config, SysConfig, Env) ->
  case elixir() of
    false ->
      {error, elixir_not_found};
    ELIXIR ->
      case elixir_config_dtl:render([]) of
        {ok, Data} ->
          TmpFile = tempfile:name("jorel", [{ext, "ex"}]),
          case file:write_file(TmpFile, Data, [write, binary]) of
            ok -> 
              Cmd = string:join([ELIXIR, TmpFile, Config, SysConfig, eutils:to_list(Env)], " "),
              eutils:to_atom(
                string:strip(
                  jorel_cmd:run(lists:flatten(Cmd)),
                  right,
                  10));
            E -> E
          end;
        E -> 
          E
      end
  end.

iex() ->
  os:find_executable("iex").

elixir() ->
  os:find_executable("elixir").

