-module(jorel_erlang_mk).

-export([exist/0,
         parse_makefile/0]).

exist() ->
  filelib:is_file("erlang.mk") and filelib:is_file("Makefile").

parse_makefile() ->
  {ok, Device} = file:open("Makefile", [read]),
  read_makefile(Device, []).

read_makefile(Device, Acc) ->
  case io:get_line(Device, "") of
    eof -> file:close(Device), Acc;
    Line -> read_makefile(Device, Acc ++ parse_line(Line, Device, false))
  end.

read_next_line(Device, Acc) ->
  case io:get_line(Device, "") of
    eof -> Acc;
    Line -> Acc ++ parse_line(Line, Device, true)
  end.

parse_line(Line, Device, IsNextLine) ->
  case string:tokens(Line, "\r\n\t =") of
    [] -> [];
    Data -> 
      [NL|NoNLData] = lists:reverse(Data),
      [Entry|Data1] = if
                        NL == "\\" -> read_next_line(Device, lists:reverse(NoNLData));
                        true -> Data
                      end,
      if
        IsNextLine -> [Entry|Data1];
        true ->
          case Entry of
            "PROJECT" -> 
              [Name] = Data1,
              [{project, Name}];
            "DEPS" -> 
              [{deps, Data1}];
            [$d, $e, $p, $_|_] = Dep ->
              [{list_to_atom(Dep), Data1}];
            _ -> 
              []
          end
      end
  end.

