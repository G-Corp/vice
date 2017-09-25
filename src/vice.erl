-module(vice).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-include("../include/vice.hrl").

%% Common API
-export([
         start/0
         , start_link/0
         , type/1
         , infos/1
         , infos/2
         , info/2
         , status/1
         , stop/1
         , convert/2
         , convert/3
         , convert/4
        ]).

-export([webvtt_finalize/2]).

%% Video API
-export([
         screenshot/2
         , webvtt/2
         , webvtt/3
         , to_html5_webm/2
         , to_html5_webm/3
         , to_html5_mp4/2
         , to_html5_mp4/3
         , to_html5_ogg/2
         , to_html5_ogg/3
        ]).

%% gen_server callbacks
-export([init/1
         , handle_call/3
         , handle_cast/2
         , handle_info/2
         , terminate/2
         , code_change/3]).

-define(SERVER, ?MODULE).

-type info_options() :: [{labels, atom | binary}].

% @doc
% Start vice application
% @end
-spec start() -> {ok, [atom()]} | {error, term()}.
start() ->
  application:ensure_all_started(vice).

% @hidden
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% @doc
% Return the file type (image or video)
% @end
-spec type(File :: file:filename_all()) -> video | image | unknow.
type(File) ->
  case bucmime:exploded(File) of
    {<<"image">>, _} -> image;
    {<<"video">>, _} -> video;
    {<<"audio">>, _} -> audio;
    _ -> unknow
  end.

% @equiv infos(File, [])
-spec infos(File :: file:filename_all()) -> {ok, term()} | {error, term()}.
infos(File) ->
  infos(File, []).

% @doc
% Return the media informations
% @end
-spec infos(File :: file:filename_all(), Options :: info_options()) -> {ok, term()} | {error, term()}.
infos(File, Options) ->
  gen_server:call(?SERVER, {infos, File, Options}).

% @doc
% Return the given media informations
% @end
-spec info(File :: file:filename_all(), Info :: atom()) -> {ok, term()} | {error, term()}.
info(File, Info) ->
  gen_server:call(?SERVER, {info, File, Info}).

% @doc
% Return the conversion status
% @end
-spec status(Worker :: reference()) -> running | {running, float()} | done.
status(Worker) ->
  gen_server:call(?SERVER, {status, Worker}).

% @doc
% Stop a running job
% @end
-spec stop(Worker :: reference()) -> ok | {error, term()}.
stop(Worker) ->
  gen_server:call(?SERVER, {stop, Worker}).

% @equiv convert(In, Out, [], undefined)
convert(In, [Opt|_] = Options) when is_tuple(Opt) orelse is_atom(Opt) ->
  convert(In, undefined, Options, undefined);
convert(In, Out) ->
  convert(In, Out, [], undefined).
% @doc
% Convert a media
% @end
convert(In, [Opt|_] = Options, Fun) when (is_tuple(Opt) orelse is_atom(Opt)),
                                         (is_function(Fun) orelse Fun == sync) ->
  convert(In, undefined, Options, Fun);
convert(In, Out, Options) when is_list(Options) ->
  convert(In, Out, Options, undefined);
convert(In, Out, Fun) when is_function(Fun) orelse Fun == sync ->
  convert(In, Out, [], Fun).
% @doc
% Convert a media
% @end
-spec convert(In :: binary() | string() | [binary() | string()],
              Out :: binary() | string() | undefined,
              Options :: list(),
              {fun((Data :: term()) -> term())
               | fun(({ok,
                       In :: string() | binary(),
                       Out :: string() | binary()}
                      | {error, term()},
                      Data :: term()) -> term()),
               Data :: term()}
              | fun(({ok,
                      In :: string() | binary(),
                      Out :: string() | binary()}
                     | {error, term()}) -> term())
              | fun(() -> term())
              | sync
              | undefined) ->
  {async, term()}
  | {ok, In :: binary() | string(), Out :: binary() | string()}
  | {error, term()}.
convert(In, Out, Options, Fun) ->
  gen_server:call(?SERVER, {convert, In, Out, Options, Fun}, infinity).

% @doc
% Create a screenshot for a movie
% @end
-spec screenshot(Movie :: binary() | string(),
                 Out :: binary() | string()) ->
  {ok, Movie :: binary() | string(), Out :: binary() | string()}
  | {error, term()}.
screenshot(Movie, Out) ->
  case info(Movie, duration) of
    {ok, Duration} ->
      Position = vice_utils:to_hms(Duration/2),
      convert(Movie, Out, [{duration, 1},
                           {output_format, "mjpeg"},
                           {input_position, Position}], sync);
    _ ->
      {error, invalid_media}
  end.

% @doc
% Generate a video thumbnails (.vtt + sprite)
%
% Options:
% <ul>
% <li><tt>every :: integer()</tt></li>
% <li><tt>width :: integer()</tt></li>
% <li><tt>out_path :: string()</tt></li>
% <li><tt>sprite :: true | false</tt></li>
% </ul>
% @end
-spec webvtt(Movie :: binary() | string(),
             OutName :: binary() | string(),
             Options :: list()) -> ok | {error, term()}.
webvtt(Movie, OutName, Options) ->
  Every = buclists:keyfind(every, 1, Options, 1),
  OutPath = buclists:keyfind(out_path, 1, Options, "."),
  SpritesPath = filename:join(OutPath, OutName),
  case bucfile:make_dir(SpritesPath) of
    ok ->
      SpritesFiles = filename:join(SpritesPath, "%016d.png"),
      convert(Movie, SpritesFiles, [{output_format, "image2"},
                                    {video_filtergraph, "fps=1/" ++ bucs:to_string(Every)}],
              {fun ?MODULE:webvtt_finalize/2, {Movie, OutName, Options}});

    Error ->
      Error
  end.

% @hidden
webvtt_finalize({ok, _, _}, {Movie, OutName, Options}) ->
  Every = buclists:keyfind(every, 1, Options, 1),
  OutPath = buclists:keyfind(out_path, 1, Options, "."),
  AssetsPath = buclists:keyfind(assets_path, 1, Options, ""),
  Width = buclists:keyfind(width, 1, Options, 150),
  Sprite = buclists:keyfind(width, 1, Options, true),
  SpritesPath = filename:join(OutPath, OutName),
  AllSprites = filename:join(SpritesPath, "*.png"),
  convert(AllSprites, [{geometry, Width}], sync),
  [F|_] = Sprites = filelib:wildcard(AllSprites),
  case {vice:infos(F), vice:info(Movie, duration)} of
    {{ok, #{page_width := Width,
            page_height := Height,
            page_x_offset := X,
            page_y_offset := Y}},
     {ok, Duration}} ->
      {Lines, Columns} = vice_utils:tile(length(Sprites)),
      case generate_vtt(OutName, OutPath, Sprites, AssetsPath, Every, Duration, Lines, Columns, Width, Height, X, Y, Sprite) of
        ok -> ok;
        Error ->
          bucfile:remove_recursive(SpritesPath),
          Error
      end;
    {{ok, _}, Error} ->
      bucfile:remove_recursive(SpritesPath),
      Error;
    {Error, {ok, _}} ->
      bucfile:remove_recursive(SpritesPath),
      Error;
    {Error, _} ->
      bucfile:remove_recursive(SpritesPath),
      Error
  end.

% @equiv webvtt(Movie, OutName, [{every, 1}, {width, 100}, {out_path, "."}, {sprite, true}, {assets_path, ""}])
webvtt(Movie, OutName) ->
  webvtt(Movie, OutName, [{every, 1}, {width, 100}, {out_path, "."}, {sprite, true}, {assets_path, ""}]).

% TODO : remove in 1.0.0
% @deprecated
to_html5_webm(Input, Output) ->
  to_html5_webm(Input, Output, undefined).
% @deprecated
to_html5_webm(Input, Output, Fun) ->
  convert(Input, Output, [{output_format, "webm"}, {vcodec, "libvpx"}, {output_acodec, "libvorbis"}, {output_frame_size, "640x360"}], Fun).
% @deprecated
to_html5_mp4(Input, Output) ->
  to_html5_mp4(Input, Output, undefined).
% @deprecated
to_html5_mp4(Input, Output, Fun) ->
  convert(Input, Output, [{output_format, "mp4"}, {vcodec, "libx264"}, {output_acodec, "libfaac"}, {output_frame_size, "640x360"}], Fun).
% @deprecated
to_html5_ogg(Input, Output) ->
  to_html5_ogg(Input, Output, undefined).
% @deprecated
to_html5_ogg(Input, Output, Fun) ->
  convert(Input, Output, [{vcodec, "libtheora"}, {output_acodec, "libvorbis"}, {output_frame_size, "640x360"}], Fun).

% @hidden
init(_) ->
  {ok, #{
     video => start_encoders(video, ?VIDEO_ENCODERS),
     image => start_encoders(image, ?PHOTO_ENCODERS),
     audio => start_encoders(audio, ?AUDIO_ENCODERS)
    }}.

% @hidden
handle_call({infos, File, Options}, _From, State) ->
  case get_encoder(File, State, false) of
    {false, {ok, Encoder}} ->
      Reply = gen_server:call(Encoder, {infos, File, Options}),
      release_encoder(Encoder),
      {reply, Reply, State};
    {true, {ok, Encoder}} ->
      release_encoder(Encoder),
      {reply, {error, unsuported}, State};
    Error ->
      {reply, Error, State}
  end;
handle_call({info, File, Info}, _From, State) ->
  case get_encoder(File, State, false) of
    {false, {ok, Encoder}} ->
      Reply = gen_server:call(Encoder, {info, File, Info}),
      release_encoder(Encoder),
      {reply, Reply, State};
    {true, {ok, Encoder}} ->
      release_encoder(Encoder),
      {reply, {error, unsuported}, State};
    Error ->
      {reply, Error, State}
  end;
handle_call({convert, In, Out, Options, Fun}, From, State) ->
  case get_encoder(In, State, false) of
    {Multi, {ok, Encoder}} ->
      gen_server:cast(Encoder, {convert, In, Out, Options, Multi, Fun, From}),
      {noreply, State};
    Error ->
      {reply, Error, State}
  end;
handle_call({status, Worker}, _From, State) ->
  case vice_prv_status:value(Worker) of
    undefined ->
      {reply, done, State};
    Value ->
      {reply, {running, Value}, State}
  end;
handle_call({stop, Worker}, _From, State) ->
  case vice_prv_status:port(Worker) of
    undefined ->
      {reply, {error, unknow_job}, State};
    Port ->
      case erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} ->
          os:cmd("kill -9 " ++ bucs:to_string(OsPid)),
          {reply, ok, State};
        _ ->
          {reply, {error, stopped_job}, State}
      end
  end;
handle_call(_Request, _From, State) ->
  {reply, {error, missing_encoder}, State}.

% @hidden
handle_cast({terminate, Encoder}, State) ->
  release_encoder(Encoder),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info(_Info, State) ->
  {noreply, State}.

% @hidden
terminate(_Reason, _State) ->
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% Private

start_encoders(Type, Default) ->
  lager:debug("Start ~p encoders...", [Type]),
  case poolgirl:add_pool(Type, {vice_prv_encoder,
                                 start_link,
                                 [Type, doteki:get_env([vice, encoders, Type], Default)]}) of
    {ok, N} ->
      lager:debug("~p ~p encoder started!", [N, Type]),
      true;
    {error, Reason} ->
      lager:error("Faild to start ~p encoder: ~p", [Type, Reason]),
      false
  end.

get_encoder([File|_], State, _) when is_list(File);
                                     is_binary(File) ->
  get_encoder(File, State, true);
get_encoder(File, State, Type) ->
  case type(File) of
    unknow ->
      {error, invalid_file};
    EncoderType ->
      case State of
        #{EncoderType := true} ->
          {Type, poolgirl:checkout(EncoderType)};
        _ ->
          {error, invalid_file}
      end
  end.

release_encoder(?MODULE) -> ok;
release_encoder(Encoder) ->
  poolgirl:checkin(Encoder).

generate_vtt(OutName, OutPath, AllSprites, AssetsPath, Every, Duration, _Lines, _Columns, Width, Height, _X, _Y, false) ->
  VttFile = filename:join(OutPath, OutName ++ ".vtt"),
  case file:open(VttFile, [write]) of
    {ok, IO} ->
      io:format(IO, "WEBVTT~n", []),
      vvtline(AllSprites, 0, Every, Duration, filename:join(AssetsPath, OutName), Width, Height, IO),
      file:close(IO);
    Error ->
      Error
  end;

generate_vtt(OutName, OutPath, AllSprites, AssetsPath, Every, Duration, Lines, Columns, Width, Height, X, Y, true) ->
  convert(AllSprites, filename:join(OutPath, OutName ++ ".png"), [{tile, Columns, Lines}, {geometry, Width, Height, X, Y}], sync),
  VttFile = filename:join(OutPath, OutName ++ ".vtt"),
  SpritesPath = filename:join(OutPath, OutName),
  bucfile:remove_recursive(SpritesPath),
  case file:open(VttFile, [write]) of
    {ok, IO} ->
      io:format(IO, "WEBVTT~n", []),
      vvtline(length(AllSprites), 0, 0, 0, Every, Duration, filename:join(AssetsPath, OutName ++ ".png"), Width, Height, Lines, Columns, IO),
      file:close(IO);
    Error ->
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
  io:format(IO, "~s/~s#xywh=0,0,~p,~p~n", [AssetsPath, filename:basename(Sprite), Width, Height]);
vvtline([Sprite|AllSprites], Start, Every, Duration, AssetsPath, Width, Height, IO) ->
  End = Start + Every,
  io:format(IO, "~n~s --> ~s~n", [vice_utils:to_full_hms(Start), vice_utils:to_full_hms(End)]),
  io:format(IO, "~s/~s#xywh=0,0,~p,~p~n", [AssetsPath, filename:basename(Sprite), Width, Height]),
  vvtline(AllSprites, End, Every, Duration, AssetsPath, Width, Height, IO).

