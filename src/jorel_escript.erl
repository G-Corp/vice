% @hidden
-module(jorel_escript).
-include("../include/jorel.hrl").

-export([build/2]).

build(Source, Dest) ->
  case compile:file(Source, [binary, return_errors]) of
    {ok, ModuleName, Binary} ->
      ModStr = atom_to_list(ModuleName),
      Beam = filename:join([ModStr, "ebin", ModStr ++ ".beam"]),
      Script = filename:join(Dest, atom_to_list(ModuleName)),
      case escript:create(Script,
                          [shebang
                           , {comment, ""}
                           , {emu_args, "-escript main " ++ ModStr ++ " -pz " ++ ModStr ++ "/ebin"}
                           , {archive, [{Beam, Binary}], []}]) of
        ok ->
          case file:change_mode(Script, 8#755) of
            ok -> Script;
            {error, Reason1} ->
              ?HALT("!!! Can't set executable to ~s: ~p", [Script, Reason1])
          end;
        {error, Reason} ->
          ?HALT("!!! Failed to create ~s: ~p", [Script, Reason])
      end;
    {error, Errors, Warnings} ->
      ?WARN("~p", [Warnings]),
      ?HALT("~p", [Errors])
  end.

