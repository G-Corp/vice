% @hidden
-module(evic_facedetect).
-export([face/2, face/1]).
-on_load(init/0).

-define(APPNAME, evic).
-define(LIBNAME, evic_facedetect).
-define(CASCADE_XML, "haarcascade_frontalface_alt2.xml").

face(Img) ->
  case buccode:priv_dir(?APPNAME) of
    error ->
      error;
    Path ->
      face(bucs:to_binary(Img), 
           bucs:to_binary(filename:join([Path, ?CASCADE_XML])))
  end.

face(_, _) ->
  not_loaded(?LINE).

init() ->
  SoName = case code:priv_dir(?APPNAME) of
             {error, bad_name} ->
               case filelib:is_dir(filename:join(["..", priv])) of
                 true ->
                   filename:join(["..", priv, ?LIBNAME]);
                 _ ->
                   filename:join([priv, ?LIBNAME])
               end;
             Dir ->
               filename:join(Dir, ?LIBNAME)
           end,
  erlang:load_nif(SoName, 0).

not_loaded(Line) ->
  exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
