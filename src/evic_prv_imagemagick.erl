% @hidden
-module(evic_prv_imagemagick).
-compile([{parse_transform, lager_transform}]).

%% API
-export([
         init/0
         , infos/2
         , info/3
         , convert/6
        ]).

-record(state, {
          convert,
          identify
         }).

-define(INFOS, "~s -format \"~s\" \"~ts\"").

init() ->
  case evic_utils:find_executable(["convert"], [evic, imagemagick, convert]) of
    undefined ->
      {stop, convert_not_found};
    Convert ->
      case evic_utils:find_executable(["identify"], [evic, imagemagick, identify]) of
        undefined ->
          {stop, identify_not_found};
        Identify ->
          {ok, #state{
                  convert = Convert,
                  identify = Identify
                 }}
      end
  end.

infos(#state{identify = Identify}, File) ->
  {ok, #{
     file_size => get_info(Identify, "%b", File, undefined),
     filename => get_info(Identify, "%f", File, undefined),
     page_geometry => get_info(Identify, "%g", File, undefined),
     height => get_info(Identify, "%h", File, fun bucs:to_integer/1),
     unique_colors => get_info(Identify, "%k", File, undefined),
     file_format => get_info(Identify, "%m", File, undefined),
     number_of_images_in_sequence => get_info(Identify, "%n", File, fun bucs:to_integer/1),
     image_index => get_info(Identify, "%p", File, fun bucs:to_integer/1),
     quantum_depth => get_info(Identify, "%q", File, fun bucs:to_integer/1),
     scene_number => get_info(Identify, "%s", File, fun bucs:to_integer/1),
     width => get_info(Identify, "%w", File, fun bucs:to_integer/1),
     x_resolution => get_info(Identify, "%x", File, fun bucs:to_integer/1),
     y_resolution => get_info(Identify, "%y", File, fun bucs:to_integer/1),
     depth => get_info(Identify, "%z", File, fun bucs:to_integer/1),
     compression_type => get_info(Identify, "%C", File, undefined),
     page_height => get_info(Identify, "%H", File, fun bucs:to_integer/1),
     compression_quality => get_info(Identify, "%Q", File, fun bucs:to_integer/1),
     time_delay => get_info(Identify, "%T", File, fun bucs:to_integer/1),
     resolution_unit => get_info(Identify, "%U", File, undefined),
     page_width => get_info(Identify, "%W", File, fun bucs:to_integer/1),
     page_x_offset => get_info(Identify, "%X", File, fun bucs:to_integer/1),
     page_y_offset => get_info(Identify, "%Y", File, fun bucs:to_integer/1)
    }}.

info(_, _, _) ->
  {error, unavailable}.

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

convert(#state{convert = Convert}, In, Out, Options, Fun, From) ->
  case Fun of
    sync ->
      ok;
    _ ->
      gen_server:reply(From, {async, self()})
  end,
  case gen_command(Convert, In, Out, Options) of
    {ok, Cmd} ->
      lager:debug("COMMAND : ~p", [Cmd]),
      case bucos:run(Cmd) of
        {ok, _} -> 
          evic_utils:reply(Fun, From, {ok, In, Out});
        Error ->
          evic_utils:reply(Fun, From, Error)
      end;
    Error ->
      evic_utils:reply(Fun, From, Error)
  end,
  gen_server:cast(evic, {terminate, self()}).

gen_command(Convert, In, Out, Options) ->
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
      {ok, format("~s \"~ts\" ~s \"~ts\"", [Convert, In, options(Options2), Out])};
    Error ->
      Error
  end.

get_face(In, W, H, Options) ->
  case evic_facedetect:first_face(In) of
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
  case evic_facedetect:first_face(In) of
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

options(Options) ->
  option(Options, []).

option([], Acc) ->
  string:join(lists:reverse(Acc), " ");

option([{resize, P, percent}|Rest], Acc) ->
  option(Rest, [format("-resize ~w%", [P])|Acc]);
option([{resize, P, pixels}|Rest], Acc) ->
  option(Rest, [format("-resize ~w@", [P])|Acc]);
option([{resize, W, H}|Rest], Acc) ->
  option(Rest, [format("-resize ~wx~w", [W, H])|Acc]);
option([{resize, W, H, percent}|Rest], Acc) ->
  option(Rest, [format("-resize ~w%x~w%", [W, H])|Acc]);
option([{resize, W, H, ignore_ration}|Rest], Acc) ->
  option(Rest, [format("-resize ~wx~w\\!", [W, H])|Acc]);
option([{resize, W, H, no_enlarge}|Rest], Acc) ->
  option(Rest, [format("-resize ~wx~w\\<", [W, H])|Acc]);
option([{resize, W, H, no_shrink}|Rest], Acc) ->
  option(Rest, [format("-resize ~wx~w\\>", [W, H])|Acc]);
option([{resize, W, H, fill}|Rest], Acc) ->
  option(Rest, [format("-resize ~wx~w\\^", [W, H])|Acc]);

option([{thumbnail, P, percent}|Rest], Acc) ->
  option(Rest, [format("-thumbnail ~w%", [P])|Acc]);
option([{thumbnail, P, pixels}|Rest], Acc) ->
  option(Rest, [format("-thumbnail ~w@", [P])|Acc]);
option([{thumbnail, W, H}|Rest], Acc) ->
  option(Rest, [format("-thumbnail ~wx~w", [W, H])|Acc]);
option([{thumbnail, W, H, percent}|Rest], Acc) ->
  option(Rest, [format("-thumbnail ~w%x~w%", [W, H])|Acc]);
option([{thumbnail, W, H, ignore_ration}|Rest], Acc) ->
  option(Rest, [format("-thumbnail ~wx~w\\!", [W, H])|Acc]);
option([{thumbnail, W, H, no_enlarge}|Rest], Acc) ->
  option(Rest, [format("-thumbnail ~wx~w\\<", [W, H])|Acc]);
option([{thumbnail, W, H, no_shrink}|Rest], Acc) ->
  option(Rest, [format("-thumbnail ~wx~w\\>", [W, H])|Acc]);
option([{thumbnail, W, H, fill}|Rest], Acc) ->
  option(Rest, [format("-thumbnail ~wx~w\\^", [W, H])|Acc]);

option([{crop, W, H, X, Y}|Rest], Acc) ->
  option(Rest, [format("-crop ~wx~w+~w+~w", [W, H, X, Y])|Acc]);
option([{crop, W, H}|Rest], Acc) ->
  option(Rest, [format("-crop ~wx~w+0+0", [W, H])|Acc]);

option([{gravity, Gravity}|Rest], Acc) ->
  option(Rest, [format("-gravity ~s", [Gravity])|Acc]);

option([repage|Rest], Acc) ->
  option(Rest, ["-repage"|Acc]);
option(['+repage'|Rest], Acc) ->
  option(Rest, ["+repage"|Acc]);

option([flip|Rest], Acc) ->
  option(Rest, ["-flip"|Acc]);

option([trim|Rest], Acc) ->
  option(Rest, ["-trim"|Acc]);

option([magnify|Rest], Acc) ->
  option(Rest, ["-magnify"|Acc]);

option([{rotate, Degrees}|Rest], Acc) ->
  option(Rest, [format("-rotate ~w", [Degrees])|Acc]);

option([{blur, Radius}|Rest], Acc) ->
  option(Rest, [format("-blur ~w", [Radius])|Acc]);
option([{blur, Radius, Sigma}|Rest], Acc) ->
  option(Rest, [format("-blur ~wx~w", [Radius, Sigma])|Acc]);

option([{edge, Radius}|Rest], Acc) ->
  option(Rest, [format("-edge ~w", [Radius])|Acc]);

option([{size, Weight}|Rest], Acc) ->
  option(Rest, [format("-size ~w", [Weight])|Acc]);
option([{size, Weight, Height}|Rest], Acc) ->
  option(Rest, [format("-size ~wx~w", [Weight, Height])|Acc]);
option([{size, Weight, Height, Offset}|Rest], Acc) ->
  option(Rest, [format("-size ~wx~w+~w", [Weight, Height, Offset])|Acc]);

option([{extent, W, H}|Rest], Acc) ->
  option(Rest, [format("-extent ~wx~w", [W, H])|Acc]);

option([_|Rest], Acc) ->
  option(Rest, Acc).

format(FMT, Args) ->
  lists:flatten(io_lib:format(FMT, Args)).

