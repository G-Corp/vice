% @hidden
-module(jorel_erlang_mk).

-export([exist/0,
         parse_makefile/1,
         gen_makefile/1]).

exist() ->
  filelib:is_file("erlang.mk") and filelib:is_file("Makefile").

parse_makefile(Makefile) ->
  {ok, Device} = file:open(Makefile, [read]),
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
  case string:tokens(Line, "\r\n\t ") of
    [] -> [{raw, Line}];
    Data -> 
      case {Data, IsNextLine} of
        {["PROJECT", "="|_], false} -> parse_line1(Data, Device, IsNextLine);
        {["DEPS", "="|_], false} -> parse_line1(Data, Device, IsNextLine);
        {[[$d, $e, $p, $_|_], "="|_], false} -> parse_line1(Data, Device, IsNextLine);
        {_, true} -> parse_line1(Data, Device, IsNextLine);
        {_, false} -> [{raw, Line}]
      end
  end.

parse_line1(Data, Device, IsNextLine) ->
  [NL|NoNLData] = lists:reverse(Data),
  Entry = if
            NL == "\\" -> read_next_line(Device, lists:reverse(NoNLData));
            true -> Data
          end,
  if
    IsNextLine -> Entry;
    true ->
      case Entry of
        ["PROJECT", "="|Data1] -> 
          [{project, Data1}];
        ["DEPS", "="|Data1] -> 
          [{deps, Data1}];
        [[$d, $e, $p, $_|_] = Dep, "="|Data1] ->
          [{list_to_atom(Dep), Data1}];
        _ -> 
          [{raw, string:join(Entry, " ")}]
      end
  end.

gen_makefile(Makefile) ->
  eutils:to_binary(
    lists:foldl(fun
                  ({raw, Line}, Acc) ->
                    Acc ++ Line;
                  ({Key, Values}, Acc) ->
                    Acc ++ key_to_str(Key) ++ " = " ++ string:join(Values, " ") ++ "\n"
                end, "", Makefile)).

key_to_str(project) -> "PROJECT";
key_to_str(deps) -> "DEPS";
key_to_str(X) -> eutils:to_string(X).

