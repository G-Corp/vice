% @hidden
-module(vice_facedetect).
-export([
         faces/3,
         faces/1,
         faces_with_eyes/1,
         first_face/1
        ]).
-on_load(init/0).

-define(APPNAME, vice).
-define(LIBNAME, vice_facedetect).
-define(CASCADE_FACE_XML, "haarcascade_frontalface_alt2.xml").
-define(CASCADE_EYES_XML, "haarcascade_eye_tree_eyeglasses.xml").

faces(Img) ->
  case buccode:priv_dir(?APPNAME) of
    error ->
      error;
    Path ->
      faces(bucs:to_binary(Img),
            bucs:to_binary(filename:join([Path, ?CASCADE_FACE_XML])),
            bucs:to_binary(filename:join([Path, ?CASCADE_EYES_XML])))
  end.

faces(_, _, _) ->
  not_loaded(?LINE).

faces_with_eyes(Img) ->
  case faces(Img) of
    {ok, Faces} ->
      case buclists:delete_if(fun(E) ->
                                  case lists:keyfind(eyes, 1,  E) of
                                    {eyes, []} ->
                                      true;
                                    _ ->
                                      false
                                  end
                              end, Faces) of
        [] ->
          {error, no_face_found};
        Result ->
          {ok, Result}
      end;
    Error ->
      Error
  end.

first_face(Img) ->
  case faces(Img) of
    {ok, [Face|_] = Faces} ->
      case buclists:delete_if(fun(E) ->
                                  case lists:keyfind(eyes, 1,  E) of
                                    {eyes, []} ->
                                      true;
                                    _ ->
                                      false
                                  end
                              end, Faces) of
        [] ->
          {ok, Face};
        [Result|_] ->
          {ok, Result}
      end;
    Error ->
      Error
  end.

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
