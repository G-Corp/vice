-define(REMARK(Fmt, Args),  io:format("~s~n", [color:white(io_lib:format(Fmt, Args))])).
-define(INFO(Fmt, Args),  io:format("~s~n", [color:green(io_lib:format(Fmt, Args))])).
-define(WARN(Fmt, Args), io:format("~s~n", [color:yellow(io_lib:format(Fmt, Args))])).
-define(ERROR(Fmt, Args), io:format("~s~n", [color:red(io_lib:format(Fmt, Args))])).
-define(HALT(Fmt, Args), ?ERROR(Fmt, Args), halt(1)).
-define(DEBUG(Fmt, Args), case os:getenv("DEBUG") of
                            "1" -> io:format("~s~n", [color:blue(io_lib:format(Fmt, Args))]);
                            _ -> ok
                          end).
-define(ASK(Fmt, Args, Prompt), erlang:apply(fun(Fmt1, Args1, Prompt1) ->
                                                 io:format("~s", [color:blue(io_lib:format(Fmt1, Args1))]),
                                                 string:strip(io:get_line(Prompt1), both, 10)
                                             end, [Fmt, Args, Prompt])).
-define(JOREL_IN, "http://jorel.in/erts/").
-define(JOREL_TMP, bucfile:expand_path(filename:join(["~", ".jorel"]))).
