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
         , info/2
         , status/1
         , convert/2
         , convert/3
         , convert/4
        ]).

%% Video API
-export([
         screenshot/2
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

% @doc
% Start vice application
% @end
start() ->
  application:ensure_all_started(vice).

% @hidden
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% @doc
% Return the file type (image or video)
% @end
type(File) ->
  case bucmime:exploded(File) of
    {<<"image">>, _} -> image;
    {<<"video">>, _} -> video;
    _ -> unknow
  end.

% @doc
% Return the media informations
% @end
infos(File) ->
  gen_server:call(?SERVER, {infos, File}).

% @doc
% Return the given media informations
% @end
info(File, Info) ->
  gen_server:call(?SERVER, {info, File, Info}).

% @doc
% Return the conversion status
% @end
status(Worker) ->
  gen_server:call(?SERVER, {status, Worker}).

% @equiv convert(In, Out, [], undefined)
convert(In, Out) ->
  convert(In, Out, [], undefined).
% @doc
% Convert a media
% @end
convert(In, Out, Options) when is_list(Options) ->
  convert(In, Out, Options, undefined);
convert(In, Out, Fun) when is_function(Fun) ->
  convert(In, Out, [], Fun).
% @doc
% Convert a media
% @end
-spec convert(In :: binary() | string(),
              Out :: binary() | string(),
              Options :: list(),
              {fun((term()) -> term()) | fun((term(), term()) -> term()), term()}
              | fun((term()) -> term())
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

% @equiv to_html5_webm(Input, Output, undefined)
to_html5_webm(Input, Output) ->
  to_html5_webm(Input, Output, undefined).
% @doc
% Convert the given movie for webm html5
% @end
to_html5_webm(Input, Output, Fun) ->
  convert(Input, Output, [{output_format, "webm"},
                          {vcodec, "libvpx"},
                          {output_acodec, "libvorbis"},
                          {output_frame_size, "640x360"}], Fun).

% @equiv to_html5_mp4(Input, Output, undefined)
to_html5_mp4(Input, Output) ->
  to_html5_mp4(Input, Output, undefined).
% @doc
% Convert the given movie for mp5 html5
% @end
to_html5_mp4(Input, Output, Fun) ->
  convert(Input, Output, [{output_format, "mp4"},
                          {vcodec, "libx264"},
                          {output_acodec, "libfaac"},
                          {output_frame_size, "640x360"}], Fun).

% @equiv to_html5_ogg(Input, Output, undefined)
to_html5_ogg(Input, Output) ->
  to_html5_ogg(Input, Output, undefined).
% @doc
% Convert the given movie for ogg html5
% @end
to_html5_ogg(Input, Output, Fun) ->
  convert(Input, Output, [{vcodec, "libtheora"},
                          {output_acodec, "libvorbis"},
                          {output_frame_size, "640x360"}], Fun).

% @hidden
init(_) ->
  {ok, #{
     video => start_encoders(video, ?VIDEO_ENCODERS),
     image => start_encoders(image, ?PHOTO_ENCODERS)
    }}.

% @hidden
handle_call({infos, File}, _From, State) ->
  case get_encoder(File, State) of
    {ok, Encoder} ->
      Reply = gen_server:call(Encoder, {infos, File}),
      release_encoder(Encoder),
      {reply, Reply, State};
    Error ->
      {reply, Error, State}
  end;
handle_call({info, File, Info}, _From, State) ->
  case get_encoder(File, State) of
    {ok, Encoder} ->
      Reply = gen_server:call(Encoder, {info, File, Info}),
      release_encoder(Encoder),
      {reply, Reply, State};
    Error ->
      {reply, Error, State}
  end;
handle_call({convert, In, Out, Options, Fun}, From, State) ->
  case get_encoder(In, State) of
    {ok, Encoder} ->
      gen_server:cast(Encoder, {convert, In, Out, Options, Fun, From}),
      {noreply, State};
    Error ->
      {reply, Error, State}
  end;
handle_call({status, Worker}, _From, State) ->
  case lists:member(Worker,
                    maps:fold(fun
                                (Type, true, Acc) ->
                                  case poolgirl:assigned(Type) of
                                    {ok, P} -> P;
                                    _ -> []
                                  end ++ Acc;
                                (_, false, Acc) ->
                                  Acc
                              end, [], State)) of
    true ->
      {reply, running, State};
    false ->
      {reply, done, State}
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
  lager:info("Start ~p encoders...", [Type]),
  case poolgirl:add_pool(Type, {vice_prv_encoder,
                                 start_link,
                                 [doteki:get_env([vice, encoders, Type], Default)]}) of
    {ok, N} ->
      lager:info("~p ~p encoder started!", [N, Type]),
      true;
    {error, Reason} ->
      lager:info("Faild to start ~p encoder: ~p", [Type, Reason]),
      false
  end.

get_encoder(File, State) ->
  case type(File) of
    unknow ->
      {error, invalid_file};
    EncoderType ->
      case State of
        #{EncoderType := true} ->
          poolgirl:checkout(EncoderType);
        _ ->
          {error, invalid_file}
      end
  end.

release_encoder(?MODULE) -> ok;
release_encoder(Encoder) ->
  poolgirl:checkin(Encoder).

