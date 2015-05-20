-define(REMARK(Fmt, Args),  io:format("~s~n", [color:white(io_lib:format(Fmt, Args))])).
-define(INFO(Fmt, Args),  io:format("~s~n", [color:green(io_lib:format(Fmt, Args))])).
-define(DEBUG(Fmt, Args), io:format("~s~n", [color:yellow(io_lib:format(Fmt, Args))])).
-define(ERROR(Fmt, Args), io:format("~s~n", [color:red(io_lib:format(Fmt, Args))])).
-define(HALT(Fmt, Args), ?ERROR(Fmt, Args), halt(1)).
-define(ASK(Fmt, Args, Prompt), erlang:apply(fun(Fmt1, Args1, Prompt1) ->
                                                 io:format("~s", [color:blue(io_lib:format(Fmt1, Args1))]),
                                                 string:strip(io:get_line(Prompt1), both, 10)
                                             end, [Fmt, Args, Prompt])).
