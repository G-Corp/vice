-module(vice_thumbnails).
-compile([{parse_transform, lager_transform}]).

-export([
         generate/2
         , generate/3
         , generate/4
        ]).

-export([
         thumbnails_finalize/2
        ]).

% @equiv generate(Movie, OutName, [{every, 1}, {width, 100}, {out_path, "."}, {sprite, true}, {assets_path, ""}], undefined)
generate(Movie, OutName) ->
  generate(Movie, OutName, [{every, 1}, {width, 100}, {out_path, "."}, {sprite, true}, {assets_path, ""}], undefined).

% @equiv generate(Movie, OutName, Options, undefined)
generate(Movie, OutName, Options) ->
  generate(Movie, OutName, Options, undefined).

% @doc
% Generate a video thumbnails (.vtt + sprite)
%
% Options:
% <ul>
% <li><tt>every :: integer()</tt></li>
% <li><tt>width :: integer()</tt></li>
% <li><tt>out_path :: string()</tt></li>
% <li><tt>assets_path :: string()</tt></li>
% <li><tt>sprite :: true | false</tt></li>
% </ul>
% @end
-spec generate(Movie :: binary() | string(),
               OutName :: binary() | string(),
               Options :: list(),
               {fun((Data :: term()) -> term())
                | fun(({ok,
                        In :: string() | binary(),
                        Out :: string() | binary()}
                       |
                       {error,
                        In :: string() | binary(),
                        Out :: string() | binary(),
                        Code :: term()},
                       Data :: term()) -> term()),
                Data :: term()}
               | fun(({ok,
                       In :: string() | binary(),
                       Out :: string() | binary()}
                      |
                      {error,
                       In :: string() | binary(),
                       Out :: string() | binary(),
                       Code :: term()}) -> term())
               | fun(() -> term())
               | sync
               | undefined) ->
  {async, reference()}
  | {ok, Out :: binary() | string()}
  | {error, term()}.
generate(Movie, OutName, Options, Fun) ->
  Every = buclists:keyfind(every, 1, Options, 1),
  OutPath = buclists:keyfind(out_path, 1, Options, "."),
  SpritesPath = filename:join(OutPath, OutName),
  ResultFunOrPid = case Fun of
                     sync ->
                       self();
                     Other ->
                       Other
                   end,

  case bucfile:make_dir(SpritesPath) of
    ok ->
      SpritesFiles = filename:join(SpritesPath, "%016d.png"),
      case vice:convert(Movie, SpritesFiles, [{output_format, "image2"},
                                         {video_filtergraph, "fps=1/" ++ bucs:to_string(Every)},
                                         {type, video}|allowed_extensions(Movie)],
                   {fun ?MODULE:thumbnails_finalize/2, {Movie, OutName, Options, ResultFunOrPid}}) of
        {async, Ref} ->
          case is_pid(ResultFunOrPid) of
            true ->
              receive
                {ok, _In, Out} -> {ok, Out};
                {error, _In, _Out, Code} -> {error, Code};
                Unknow -> {error, {invalid_response, Unknow}}
              end;
            false ->
              {async, Ref}
          end;
        Error ->
          Error
      end;
    Error ->
      Error
  end.

% @hidden
thumbnails_finalize({ok, In, _Out}, {Movie, OutName, Options, FunOrPid}) ->
  Every = buclists:keyfind(every, 1, Options, 1),
  OutPath = buclists:keyfind(out_path, 1, Options, "."),
  AssetsPath = buclists:keyfind(assets_path, 1, Options, ""),
  Width = buclists:keyfind(width, 1, Options, 150),
  Sprite = buclists:keyfind(sprite, 1, Options, true),
  SpritesPath = filename:join(OutPath, OutName),
  AllSprites = filename:join(SpritesPath, "*.png"),
  vice:convert(AllSprites, [{geometry, Width}], sync),
  [F|_] = Sprites = filelib:wildcard(AllSprites),
  Response = case {vice:infos(F), vice:info(Movie, duration, [{type, video}|allowed_extensions(Movie)])} of
    {{ok, #{page_width := Width,
            page_height := Height,
            page_x_offset := X,
            page_y_offset := Y}},
     {ok, Duration}} ->
      {Lines, Columns} = vice_utils:tile(length(Sprites)),
      case generate_vtt(OutName, OutPath, Sprites, AssetsPath, Every, Duration, Lines, Columns, Width, Height, X, Y, Sprite) of
        {ok, VttFile} ->
          {ok, In, VttFile};
        Error ->
          lager:error("Failed to generate thumbnails VTT: ~p", [Error]),
          bucfile:remove_recursive(SpritesPath),
          {error, In, OutName, output_vtt_error}
      end;
    {{ok, _}, Error} ->
      lager:error("Failed to retrieve video duration when generating thumbnails: ~p", [Error]),
      bucfile:remove_recursive(SpritesPath),
      {error, In, OutName, source_duration_unavailable};
    {Error, _} ->
      lager:error("Failed to retrieve sprite infos when generating thumbnails: ~p", [Error]),
      bucfile:remove_recursive(SpritesPath),
      {error, In, OutName, sprite_creation_error}
  end,
  send_response(Response, FunOrPid);
thumbnails_finalize({error, _, _, Code} = Result, {_, _, _, FunOrPid}) ->
  lager:error("Failed to generate thumbnails: ~p", [Code]),
  send_response(Result, FunOrPid).

send_response(Response, Pid) when is_pid(Pid) ->
  Pid ! Response;
send_response(Response, {Fun, Metadatas}) when is_function(Fun, 2) ->
  erlang:apply(Fun, [Response, Metadatas]);
send_response(Response, Fun) when is_function(Fun, 1) ->
  erlang:apply(Fun, [Response]);
send_response(_Response, Fun) when is_function(Fun, 0) ->
  erlang:apply(Fun, []);
send_response(_Response, _Other) ->
  ok.

allowed_extensions(Movie) ->
  case bucstring:lowercase(bucs:to_string(filename:extension(Movie))) of
    ".m3u8" -> [{allowed_extensions, "ALL"}];
    ".mpd" -> [{allowed_extensions, "ALL"}];
    _Ext -> []
  end.

generate_vtt(OutName, OutPath, AllSprites, AssetsPath, Every, Duration, _Lines, _Columns, Width, Height, _X, _Y, false) ->
  VttFile = filename:join(OutPath, OutName ++ ".vtt"),
  lager:debug("Will generate ~ts", [VttFile]),
  case file:open(VttFile, [write]) of
    {ok, IO} ->
      io:format(IO, "WEBVTT~n", []),
      vvtline(AllSprites, 0, Every, Duration, join_file(AssetsPath, OutName), Width, Height, IO),
      file:close(IO),
      {ok, VttFile};
    Error ->
      lager:error("Failed to create thumbnails VTT file: ~p", [Error]),
      Error
  end;

generate_vtt(OutName, OutPath, AllSprites, AssetsPath, Every, Duration, Lines, Columns, Width, Height, X, Y, true) ->
  vice:convert(AllSprites, filename:join(OutPath, OutName ++ ".png"), [{tile, Columns, Lines}, {geometry, Width, Height, X, Y}], sync),
  VttFile = filename:join(OutPath, OutName ++ ".vtt"),
  SpritesPath = filename:join(OutPath, OutName),
  bucfile:remove_recursive(SpritesPath),
  case file:open(VttFile, [write]) of
    {ok, IO} ->
      io:format(IO, "WEBVTT~n", []),
      vvtline(length(AllSprites), 0, 0, 0, Every, Duration, join_file(AssetsPath, OutName ++ ".png"), Width, Height, Lines, Columns, IO),
      file:close(IO),
      {ok, VttFile};
    Error ->
      lager:error("Failed to create thumbnails VTT file: ~p", [Error]),
      Error
  end.

vvtline(0, _, _, _, _, _, _, _, _, _, _, _) ->
  ok;
vvtline(N, L, C, Start, Every, Duration, SpriteFile, Width, Height, Lines, Columns, IO) ->
  End = Start + Every,
  X = C * Width,
  Y = L * Height,
  io:format(IO, "~n~s --> ~s~n", [vice_utils:to_full_hms(Start), vice_utils:to_full_hms(End)]),
  io:format(IO, "~s#xywh=~p,~p,~p,~p~n", [SpriteFile, X, Y, Width, Height]),
  {NL, NC} = new_position(L, C, Lines, Columns),
  vvtline(N - 1, NL, NC, End, Every, Duration, SpriteFile, Width, Height, Lines, Columns, IO).

new_position(L, C, _Lines, Columns) when C == Columns ->
  {L + 1, 0};
new_position(L, C, _Lines, _Columns) ->
  {L, C + 1}.

vvtline([Sprite], Start, _Every, Duration, AssetsPath, Width, Height, IO) ->
  io:format(IO, "~n~s --> ~s~n", [vice_utils:to_full_hms(Start), vice_utils:to_full_hms(Duration)]),
  io:format(IO, "~s#xywh=0,0,~p,~p~n", [join_uri(AssetsPath, filename:basename(Sprite)), Width, Height]);
vvtline([Sprite|AllSprites], Start, Every, Duration, AssetsPath, Width, Height, IO) ->
  End = Start + Every,
  io:format(IO, "~n~s --> ~s~n", [vice_utils:to_full_hms(Start), vice_utils:to_full_hms(End)]),
  io:format(IO, "~s#xywh=0,0,~p,~p~n", [join_uri(AssetsPath, filename:basename(Sprite)), Width, Height]),
  vvtline(AllSprites, End, Every, Duration, AssetsPath, Width, Height, IO).

join_file(Path, File) ->
  case file:get_cwd() of
    {ok, Cwd} ->
      case bucs:to_string(bucfile:expand_path(Path)) of
        Cwd -> File;
        _Other -> filename:join(Path, File)
      end;
    {error, _Reason} ->
      filename:join(Path, File)
  end.

join_uri("", File) -> File;
join_uri(Path, File) when is_list(Path), is_list(File) ->
  bucuri:join(Path, File);
join_uri(Path, File) ->
  join_uri(bucs:to_string(Path), bucs:to_string(File)).
