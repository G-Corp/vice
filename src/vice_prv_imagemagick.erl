% @hidden
-module(vice_prv_imagemagick).
-include_lib("bucs/include/bucs.hrl").
-behaviour(vice_encoder).

%% API
-export([
         init/0
         , infos/3
         , info/4
         , command/5
         , progress/2
        ]).

-record(state, {
          convert,
          identify,
          mogrify,
          montage
         }).

-define(INFOS, "~s -format \"~s\" \"~ts\"").

init() ->
  case vice_utils:find_tools(record_info(fields, state), imagemagick) of
    {error, Reason} ->
      {stop, Reason};
    {state, Data} ->
      {ok, ?list_to_record(state, Data)}
  end.

infos(#state{identify = Identify}, File, Options) ->
  Response = #{
    file_size => get_info(Identify,
                          "%b",
                           File,
                           fun(Value) ->
                               bucs:to_integer(re:replace(Value, "[^0-9]*$", "", [{return, list}]))
                           end),
     file_size_unit => get_info(Identify,
                                "%b",
                                File,
                                fun(Value) ->
                                    re:replace(Value, "^[0-9]*", "", [{return, binary}])
                                end),
     filename => get_info(Identify, "%f", File, fun bucs:to_binary/1),
     page_geometry => get_info(Identify, "%g", File, fun bucs:to_binary/1),
     height => get_info(Identify, "%h", File, fun bucs:to_integer/1),
     % unique_colors => get_info(Identify, "%k", File, fun bucs:to_binary/1),
     file_format => get_info(Identify, "%m", File, fun bucs:to_binary/1),
     number_of_images_in_sequence => get_info(Identify, "%n", File, fun bucs:to_integer/1),
     image_index => get_info(Identify, "%p", File, fun bucs:to_integer/1),
     quantum_depth => get_info(Identify, "%q", File, fun bucs:to_integer/1),
     scene_number => get_info(Identify, "%s", File, fun bucs:to_integer/1),
     width => get_info(Identify, "%w", File, fun bucs:to_integer/1),
     x_resolution => get_info(Identify, "%x", File, fun bucs:to_integer/1),
     y_resolution => get_info(Identify, "%y", File, fun bucs:to_integer/1),
     depth => get_info(Identify, "%z", File, fun bucs:to_integer/1),
     compression_type => get_info(Identify, "%C", File, fun bucs:to_binary/1),
     page_height => get_info(Identify, "%H", File, fun bucs:to_integer/1),
     compression_quality => get_info(Identify, "%Q", File, fun bucs:to_integer/1),
     time_delay => get_info(Identify, "%T", File, fun bucs:to_integer/1),
     resolution_unit => get_info(Identify, "%U", File, fun bucs:to_binary/1),
     page_width => get_info(Identify, "%W", File, fun bucs:to_integer/1),
     page_x_offset => get_info(Identify, "%X", File, fun bucs:to_integer/1),
     page_y_offset => get_info(Identify, "%Y", File, fun bucs:to_integer/1)
    },
  case buclists:keyfind(labels, 1, Options, atom) of
    binary ->
      {ok, maps:fold(fun(K, V, Acc) ->
                         maps:put(bucs:to_binary(K), V, Acc)
                     end, #{}, Response)};
    _ ->
      {ok, Response}
  end.

info(State, File, Info, Options) ->
  case infos(State, File, Options) of
    {ok, #{Info := Value}} ->
      {ok, Value};
    _ ->
      {error, unavailable}
  end.

get_info(Identify, Attr, File, Fun) ->
  Cmd = lists:flatten(io_lib:format(?INFOS, [Identify, Attr, File])),
  case bucos:run(Cmd) of
    {ok, Data} ->
      case Fun of
        undefined -> Data;
        _ -> erlang:apply(Fun, [Data])
      end;
    _ ->
      undefined
  end.

command(State, In, undefined, Options, _Multi) ->
  gen_command(mogrify, State, In, undefined, Options);
command(State, In, Out, Options, false) ->
  gen_command(convert, State, In, Out, Options);
command(State, In, Out, Options, true) ->
  gen_command(montage, State, In, Out, Options).

gen_command(convert, #state{convert = Convert}, In, Out, Options) ->
  Options1 = case lists:keyfind(face, 1, Options) of
               {face, W, H} ->
                 get_face(In, W, H, Options);
               {face, W, H, eyes} ->
                 get_face_on_eyes(In, W, H, Options);
               _ ->
                 {ok, Options}
             end,
  case Options1 of
    {ok, Options2} ->
      {ok, format("~s \"~ts\" ~s \"~ts\"", [Convert, In, convert_options(Options2), Out])};
    Error ->
      Error
  end;
gen_command(montage, #state{montage = Montage}, In, Out, Options) ->
  {ok, format("~s ~ts ~s \"~ts\"", [Montage, string:join([io_lib:format("~p", [I]) || I <- In], " "), montage_options(Options), Out])};
gen_command(mogrify, #state{mogrify = Mogrify}, In, undefined, Options) ->
  {ok, format("~s ~s \"~ts\"", [Mogrify, mogrify_options(Options), In])}.

get_face(In, W, H, Options) ->
  case vice_facedetect:first_face(In) of
    {ok, Position} ->
      #{x := X,
        y := Y,
        width := Width,
        height := Height} = maps:from_list(Position),
      X1 = X + (Width/2) - (W / 2),
      Y1 = Y + (Height/2) - (H / 2),
      {ok, lists:keyreplace(face, 1, Options, {crop, W, H, X1, Y1})};
    Error ->
      Error
  end.

get_face_on_eyes(In, W, H, Options) ->
  case vice_facedetect:first_face(In) of
    {ok, Position} ->
      {X1, Y1} = case maps:from_list(Position) of
                   #{x := X,
                     y := Y,
                     width := Width,
                     height := _Height,
                     eyes := [Eye]} ->
                     #{x := _YX1,
                       y := YY1,
                       width := _YW1,
                       height := YH1} = maps:from_list(Eye),
                     YY = YY1 + (YH1/2),
                     {X + (Width/2) - (W / 2), Y + YY - (H / 2)};
                   #{x := X,
                     y := Y,
                     width := _Width,
                     height := _Height,
                     eyes := [Eye1, Eye2]} ->
                     #{x := YX1,
                       y := YY1,
                       width := YW1,
                       height := YH1} = maps:from_list(Eye1),
                     YX11 = YX1 + (YW1/2),
                     YY11 = YY1 + (YH1/2),
                     #{x := YX2,
                       y := YY2,
                       width := YW2,
                       height := YH2} = maps:from_list(Eye2),
                     YX21 = YX2 + (YW2/2),
                     YY21 = YY2 + (YH2/2),
                     YX = (min(YX11, YX21) + max(YX11, YX21))/2,
                     YY = (min(YY11, YY21) + max(YY11, YY21))/2,
                     {X + YX - (W / 2), Y + YY - (H / 2)};
                   #{x := X,
                     y := Y,
                     width := Width,
                     height := Height} ->
                     {X + (Width/2) - (W / 2), Y + (Height/2) - (H / 2)}
                 end,
      {ok, lists:keyreplace(face, 1, Options, {crop, W, H, X1, Y1})};
    Error ->
      Error
  end.

% convert options

convert_options(Options) ->
  convert_option(Options, ["-monitor"]).

convert_option([], Acc) ->
  string:join(lists:reverse(Acc), " ");

convert_option([{resize, P, percent}|Rest], Acc) ->
  convert_option(Rest, [format("-resize ~w%", [P])|Acc]);
convert_option([{resize, P, pixels}|Rest], Acc) ->
  convert_option(Rest, [format("-resize ~w@", [P])|Acc]);
convert_option([{resize, W, H}|Rest], Acc) ->
  convert_option(Rest, [format("-resize ~wx~w", [W, H])|Acc]);
convert_option([{resize, W, H, percent}|Rest], Acc) ->
  convert_option(Rest, [format("-resize ~w%x~w%", [W, H])|Acc]);
convert_option([{resize, W, H, ignore_ratio}|Rest], Acc) ->
  convert_option(Rest, [format("-resize ~wx~w\\!", [W, H])|Acc]);
convert_option([{resize, W, H, no_enlarge}|Rest], Acc) ->
  convert_option(Rest, [format("-resize ~wx~w\\<", [W, H])|Acc]);
convert_option([{resize, W, H, no_shrink}|Rest], Acc) ->
  convert_option(Rest, [format("-resize ~wx~w\\>", [W, H])|Acc]);
convert_option([{resize, W, H, fill}|Rest], Acc) ->
  convert_option(Rest, [format("-resize ~wx~w\\^", [W, H])|Acc]);

convert_option([{thumbnail, P, percent}|Rest], Acc) ->
  convert_option(Rest, [format("-thumbnail ~w%", [P])|Acc]);
convert_option([{thumbnail, P, pixels}|Rest], Acc) ->
  convert_option(Rest, [format("-thumbnail ~w@", [P])|Acc]);
convert_option([{thumbnail, W, H}|Rest], Acc) ->
  convert_option(Rest, [format("-thumbnail ~wx~w", [W, H])|Acc]);
convert_option([{thumbnail, W, H, percent}|Rest], Acc) ->
  convert_option(Rest, [format("-thumbnail ~w%x~w%", [W, H])|Acc]);
convert_option([{thumbnail, W, H, ignore_ratio}|Rest], Acc) ->
  convert_option(Rest, [format("-thumbnail ~wx~w\\!", [W, H])|Acc]);
convert_option([{thumbnail, W, H, no_enlarge}|Rest], Acc) ->
  convert_option(Rest, [format("-thumbnail ~wx~w\\<", [W, H])|Acc]);
convert_option([{thumbnail, W, H, no_shrink}|Rest], Acc) ->
  convert_option(Rest, [format("-thumbnail ~wx~w\\>", [W, H])|Acc]);
convert_option([{thumbnail, W, H, fill}|Rest], Acc) ->
  convert_option(Rest, [format("-thumbnail ~wx~w\\^", [W, H])|Acc]);

convert_option([{quality, Q}|Rest], Acc) ->
  convert_option(Rest, [format("-quality ~w", [Q]) | Acc]);

convert_option([{crop, W, H, X, Y}|Rest], Acc) ->
  convert_option(Rest, [format("-crop ~wx~w+~w+~w", [W, H, X, Y])|Acc]);
convert_option([{crop, W, H}|Rest], Acc) ->
  convert_option(Rest, [format("-crop ~wx~w+0+0", [W, H])|Acc]);

convert_option([{gravity, Gravity}|Rest], Acc) ->
  convert_option(Rest, [format("-gravity ~s", [Gravity])|Acc]);

convert_option([repage|Rest], Acc) ->
  convert_option(Rest, ["-repage"|Acc]);
convert_option(['+repage'|Rest], Acc) ->
  convert_option(Rest, ["+repage"|Acc]);

convert_option([flip|Rest], Acc) ->
  convert_option(Rest, ["-flip"|Acc]);

convert_option([trim|Rest], Acc) ->
  convert_option(Rest, ["-trim"|Acc]);

convert_option([magnify|Rest], Acc) ->
  convert_option(Rest, ["-magnify"|Acc]);

convert_option([{rotate, Degrees}|Rest], Acc) ->
  convert_option(Rest, [format("-rotate ~w", [Degrees])|Acc]);

convert_option(['auto-orient'|Rest], Acc) ->
  convert_option(Rest, ["-auto-orient"|Acc]);

convert_option([strip|Rest], Acc) ->
  convert_option(Rest, ["-strip"|Acc]);

convert_option([{blur, Radius}|Rest], Acc) ->
  convert_option(Rest, [format("-blur ~w", [Radius])|Acc]);
convert_option([{blur, Radius, Sigma}|Rest], Acc) ->
  convert_option(Rest, [format("-blur ~wx~w", [Radius, Sigma])|Acc]);

convert_option([{edge, Radius}|Rest], Acc) ->
  convert_option(Rest, [format("-edge ~w", [Radius])|Acc]);

convert_option([{size, Weight}|Rest], Acc) ->
  convert_option(Rest, [format("-size ~w", [Weight])|Acc]);
convert_option([{size, Weight, Height}|Rest], Acc) ->
  convert_option(Rest, [format("-size ~wx~w", [Weight, Height])|Acc]);
convert_option([{size, Weight, Height, Offset}|Rest], Acc) ->
  convert_option(Rest, [format("-size ~wx~w+~w", [Weight, Height, Offset])|Acc]);

convert_option([{extent, W, H}|Rest], Acc) ->
  convert_option(Rest, [format("-extent ~wx~w", [W, H])|Acc]);

convert_option([_|Rest], Acc) ->
  convert_option(Rest, Acc).

% mogrify options

mogrify_options(Options) ->
  mogrify_option(Options, ["-monitor"]).

mogrify_option([], Acc) ->
  string:join(lists:reverse(Acc), " ");
mogrify_option([Option|Rest], Acc) when element(1, Option) == geometry ->
  mogrify_option(Rest, [format("-geometry ~ts", [geometry(Option)])|Acc]);

mogrify_option([_|Rest], Acc) ->
  mogrify_option(Rest, Acc).

% montage options

montage_options(Options) ->
  montage_option(Options, ["-monitor"]).

montage_option([], Acc) ->
  string:join(lists:reverse(Acc), " ");
montage_option([Option|Rest], Acc) when element(1, Option) == geometry ->
  montage_option(Rest, [format("-geometry ~ts", [geometry(Option)])|Acc]);
montage_option([Option|Rest], Acc) when element(1, Option) == tile ->
  montage_option(Rest, [format("-tile ~ts", [geometry(Option)])|Acc]);

montage_option([_|Rest], Acc) ->
  montage_option(Rest, Acc).

% Common geometry

geometry({_, Scale, percent}) ->
  format("~w%", [Scale]);
geometry({_, Area, pixels}) ->
  format("~w@", [Area]);
geometry({_, ScaleX, ScaleY, percent}) ->
  format("~w%x~w%", [ScaleX, ScaleY]);
geometry({_, Width}) ->
  format("~w", [Width]);
geometry({_, Width, undefined}) ->
  format("~w", [Width]);
geometry({_, undefined, Height}) ->
  format("x~w", [Height]);
geometry({_, Width, Height}) ->
  format("~wx~w", [Width, Height]);
geometry({_, Width, Height, ignore_ratio}) ->
  format("~wx~w\\!", [Width, Height]);
geometry({_, Width, Height, no_enlarge}) ->
  format("~wx~w\\<", [Width, Height]);
geometry({_, Width, Height, no_shrink}) ->
  format("~wx~w\\>", [Width, Height]);
geometry({_, Width, Height, fill}) ->
  format("~wx~w\\^", [Width, Height]);
geometry({_, Width, Height, X, Y}) when X >= 0, Y >= 0->
  format("\"~wx~w+~w+~w\"", [Width, Height, X, Y]);
geometry({_, Width, Height, X, Y}) when X < 0, Y >= 0->
  format("\"~wx~w~w+~w\"", [Width, Height, X, Y]);
geometry({_, Width, Height, X, Y}) when X >= 0, Y < 0->
  format("\"~wx~w+~w~w\"", [Width, Height, X, Y]);
geometry({_, Width, Height, X, Y}) when X < 0, Y < 0->
  format("\"~wx~w~w~w\"", [Width, Height, X, Y]).

% format option

format(FMT, Args) ->
  lists:flatten(io_lib:format(FMT, Args)).

progress(Bytes, Sofar) ->
  case re:run(Bytes, "([0-9]*)%", [{capture, all_but_first, list}]) of
    {match, [Percent]} ->
      {undefined, undefined, bucs:to_float(Percent)};
    _ ->
      Sofar
  end.
