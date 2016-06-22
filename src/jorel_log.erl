-module(jorel_log).
-include("../include/jorel.hrl").

-export([remark/2
         , info/2
         , debug/2
         , warn/2
         , error/2
         , halt/2
         , ask/3]).

remark(Fmt, Args) -> ?REMARK(Fmt, Args).
info(Fmt, Args) -> ?INFO(Fmt, Args).
debug(Fmt, Args) -> ?DEBUG(Fmt, Args).
warn(Fmt, Args) -> ?WARN(Fmt, Args).
error(Fmt, Args) -> ?ERROR(Fmt, Args).
halt(Fmt, Args) -> ?HALT(Fmt, Args).
ask(Fmt, Args, Prompt) -> ?ASK(Fmt, Args, Prompt).

