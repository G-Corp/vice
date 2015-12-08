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
        {["PROJECT_DESCRIPTION", "="|_], false} -> parse_line1(Data, Device, IsNextLine);
        {["PROJECT_VERSION", "="|_], false} -> parse_line1(Data, Device, IsNextLine);
        {["DEPS", "="|_], false} -> parse_line1(Data, Device, IsNextLine);
        {["BUILD_DEPS", "="|_], false} -> parse_line1(Data, Device, IsNextLine);
        {["LOCAL_DEPS", "="|_], false} -> parse_line1(Data, Device, IsNextLine);
        {["TEST_DEPS", "="|_], false} -> parse_line1(Data, Device, IsNextLine);
        {["DOC_DEPS", "="|_], false} -> parse_line1(Data, Device, IsNextLine);
        {["REL_DEPS", "="|_], false} -> parse_line1(Data, Device, IsNextLine);
        {["SHELL_DEPS", "="|_], false} -> parse_line1(Data, Device, IsNextLine);
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
        ["PROJECT_DESCRIPTION", "="|Data1] -> 
          [{project_description, Data1}];
        ["PROJECT_VERSION", "="|Data1] -> 
          [{project_version, Data1}];
        ["DEPS", "="|Data1] -> 
          [{deps, Data1}];
        ["BUILD_DEPS", "="|Data1] -> 
          [{build_deps, Data1}];
        ["LOCAL_DEPS", "="|Data1] -> 
          [{local_deps, Data1}];
        ["TEST_DEPS", "="|Data1] -> 
          [{test_deps, Data1}];
        ["DOC_DEPS", "="|Data1] -> 
          [{doc_deps, Data1}];
        ["REL_DEPS", "="|Data1] -> 
          [{rel_deps, Data1}];
        ["SHELL_DEPS", "="|Data1] -> 
          [{shell_deps, Data1}];
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
key_to_str(project_description) -> "PROJECT_DESCRIPTION";
key_to_str(project_version) -> "PROJECT_VERSION";
key_to_str(deps) -> "DEPS";
key_to_str(build_deps) -> "BUILD_DEPS";
key_to_str(local_deps) -> "LOCAL_DEPS";
key_to_str(test_deps) -> "TEST_DEPS";
key_to_str(doc_deps) -> "DOC_DEPS";
key_to_str(rel_deps) -> "REL_DEPS";
key_to_str(shell_deps) -> "SHELL_DEPS";
key_to_str(X) -> eutils:to_string(X).

