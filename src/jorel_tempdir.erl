%% @doc
%% @author Gregoire Lejeune <gl@finexkap.com>
%% @copyright 2014 Finexkap
%%
%% Erlang module for managing temporary files
%% @end
-module(jorel_tempdir).
-include_lib("kernel/include/file.hrl").

-export([
         name/0,
         name/1,
         mktmp/1,
         mktmp/2
        ]).

-define(CHARS, "azertyuiopqsdfghjklmwxcvbnAZERTYUIOPQSDFGHJKLMWXCVBN1234567890").

-type tmpname_options() :: [tmpname_option()].
-type tmpname_option() :: {prefix, string()} | {path, string()}.

name() ->
  name([]).
-spec name(tmpname_options()) -> string().
name(Options) ->
  Options1 = maps:from_list(Options),
  Prefix = maps:get(prefix, Options1, "tmp_"),
  Path = maps:get(path, Options1, dir()),
  filename:join([Path, Prefix ++ randstr(20)]).

mktmp(Fun) ->
  mktmp([], Fun).
mktmp(Options, Fun) when is_list(Options), is_function(Fun, 1) ->
  Dir = name(Options),
  case efile:make_dir(Dir) of
    ok ->
      Result = Fun(Dir),
      _ = case elists:keyfind(remove, 1, Options, true) of
          true -> efile:remove_recursive(Dir);
          _ -> ok
        end,
      Result;
    E -> E
  end.

% Private

-spec dir() -> string() | false.
dir() ->
  case os:getenv("TMPDIR") of
    false ->
      case os:getenv("TEMP") of
        false ->
          case os:getenv("TMP") of
            false ->
              case write_tmp_dir("/tmp") of
                false ->
                  Cwd = case file:get_cwd() of
                          {ok, Dir} -> Dir;
                          _ -> "."
                        end,
                  case write_tmp_dir(Cwd) of
                    false -> false;
                    LTmp -> LTmp
                  end;
                STmp -> STmp
              end;
            Tmp -> Tmp
          end;
        Temp -> Temp
      end;
    Tmpdir -> Tmpdir
  end.

write_tmp_dir(Path) ->
  case file:read_file_info(Path) of
    {ok, #file_info{type = directory, access = Access}} when Access =:= read_write; Access =:= write ->
      Path;
    _ -> false
  end.

randstr(Size) ->
  [lists:sublist(?CHARS, random:uniform(length(?CHARS)), 1) || _ <- lists:seq(1, Size)].
