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
         , info/3
         , status/1
         , stop/1
         , convert/2
         , convert/3
         , convert/4
        ]).

%% Video API
-export([
         screenshot/2
        ]).

%% DEPRECATED -- REMOVE  in 1.0.0
-export([
         webvtt/2
         , webvtt/3
         , to_html5_webm/2
         , to_html5_webm/3
         , to_html5_mp4/2
         , to_html5_mp4/3
         , to_html5_ogg/2
         , to_html5_ogg/3
         , thumbnails/2
         , thumbnails/3
        ]).

%% gen_server callbacks
-export([init/1
         , handle_call/3
         , handle_cast/2
         , handle_info/2
         , terminate/2
         , code_change/3]).

-define(SERVER, ?MODULE).

-type info_option() :: {labels, atom | binary}
                       | {type, image | audio | video}
                       | {allowed_extensions, string()}.
-type info_options() :: [info_option()]
                        | #{labels => atom | binary,
                            type => image | audio | video,
                            allowed_extensions => string()}.

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
% Return the file type (image, audio or video)
% @end
-spec type(File :: file:filename_all()) -> audio | video | image | unknow.
type(File) ->
  case type_by_ext(File) of
    unknow ->
      case file_signatures:signature(File) of
        {error, _Error} ->
          unknow;
        undefined ->
          unknow;
        Type ->
          type_by_ext(<<"x.", (bucs:to_binary(Type))/binary>>)
      end;
    Type ->
      Type
  end.

type_by_ext(File) ->
  case bucmime:exploded(File) of
    {Type, _} ->
      AType = bucs:to_atom(Type),
      case lists:member(AType, [audio, video, image]) of
        true ->
          AType;
        false ->
          unknow
      end;
    _ -> unknow
  end.

type(File, Options) ->
  case lists:keyfind(type, 1, Options) of
    {type, Type} ->
      Type;
    false ->
      type(File)
  end.

% @equiv infos(File, [])
-spec infos(File :: file:filename_all()) -> {ok, term()} | {error, term()}.
infos(File) ->
  infos(File, []).

% @doc
% Return all media informations
% @end
-spec infos(File :: file:filename_all(), Options :: info_options()) -> {ok, term()} | {error, term()}.
infos(File, Options) when is_list(Options) ->
  gen_server:call(?SERVER, {infos, File, Options});
infos(File, Options) when is_map(Options) ->
  infos(File, maps:to_list(Options)).

% @equiv info(File, Info, [])
-spec info(File :: file:filename_all(), Info :: atom()) -> {ok, term()} | {error, term()}.
info(File, Info) ->
  info(File, Info, []).

% @doc
% Return the given media information
% @end
-spec info(File :: file:filename_all(), Info :: atom(), Options :: info_options()) -> {ok, term()} | {error, term()}.
info(File, Info, Options) ->
  gen_server:call(?SERVER, {info, File, Info, Options}).

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

% @hidden
init(_) ->
  init(
    buclists:merge_keylists(
      1,
      doteki:get_env([vice, encoders], []),
      maps:to_list(?DEFAULT_ENCODERS)),
    #{}).

init([], Acc) -> {ok, Acc};
init([{Type, _}|Rest], Acc) ->
  init(Rest, maps:put(Type, start_encoders(Type), Acc)).

% @hidden
handle_call({infos, File, Options}, _From, State) ->
  case get_encoder(File, State, false, Options) of
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
handle_call({info, File, Info, Options}, _From, State) ->
  case get_encoder(File, State, false, Options) of
    {false, {ok, Encoder}} ->
      Reply = gen_server:call(Encoder, {info, File, Info, Options}),
      release_encoder(Encoder),
      {reply, Reply, State};
    {true, {ok, Encoder}} ->
      release_encoder(Encoder),
      {reply, {error, unsuported}, State};
    Error ->
      {reply, Error, State}
  end;
handle_call({convert, In, Out, Options, Fun}, From, State) ->
  case get_encoder(In, State, false, Options) of
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

start_encoders(Type) ->
  lager:debug("Start ~p encoder...", [Type]),
  case doteki:get_env([vice, encoders, Type], maps:get(Type, ?DEFAULT_ENCODERS, undefined)) of
    undefined ->
          lager:error("Encoder for ~p undefined.", [Type]),
          false;
    Encoders ->
      case poolgirl:add_pool(Type, {vice_prv_encoder,
                                    start_link,
                                    [Type, Encoders]},
                             #{allow_empty_pool => false}) of
        {ok, N} ->
          lager:debug("~p ~p encoder started!", [N, Type]),
          true;
        {error, Reason} ->
          lager:error("Faild to start ~p encoder: ~p", [Type, Reason]),
          false
      end
  end.

get_encoder([File|_], State, _, Options) when is_list(File);
                                              is_binary(File) ->
  get_encoder(File, State, true, Options);
get_encoder(File, State, Type, Options) ->
  case type(File, Options) of
    unknow ->
      {error, invalid_file};
    EncoderType ->
      case State of
        #{EncoderType := true} ->
          {Type, poolgirl:checkout(EncoderType)};
        _ ->
          {error, {encoder_not_found, EncoderType}}
      end
  end.

release_encoder(?MODULE) -> ok;
release_encoder(Encoder) ->
  poolgirl:checkin(Encoder).

% DEPRECATED FUNTIONS --------------------------------------------------------------------------------------------------------
% TODO : remove in 1.0.0

% @deprecated use vice_thumbnails:generate/2
webvtt(Movie, OutName) -> thumbnails(Movie, OutName).

% @deprecated use vice_thumbnails:generate/3
webvtt(Movie, OutName, Options) -> thumbnails(Movie, OutName, Options).

% @deprecated use vice_thumbnails:generate/2
thumbnails(Movie, OutName) ->
  vice_thumbnails:generate(Movie, OutName).

% @deprecated use vice_thumbnails:thumbnails/3
thumbnails(Movie, OutName, Options) ->
  vice_thumbnails:generate(Movie, OutName, Options).

% @deprecated use convert/4 with <tt>html5webm</tt> preset
to_html5_webm(Input, Output) ->
  to_html5_webm(Input, Output, undefined).
% @deprecated use convert/4 with <tt>html5webm</tt> preset
to_html5_webm(Input, Output, Fun) ->
  convert(Input, Output, [{output_format, "webm"}, {vcodec, "libvpx"}, {output_acodec, "libvorbis"}, {output_frame_size, "640x360"}], Fun).
% @deprecated use convert/4 with <tt>html5mp4</tt> preset
to_html5_mp4(Input, Output) ->
  to_html5_mp4(Input, Output, undefined).
% @deprecated use convert/4 with <tt>html5mp4</tt> preset
to_html5_mp4(Input, Output, Fun) ->
  convert(Input, Output, [{output_format, "mp4"}, {vcodec, "libx264"}, {output_acodec, "libfaac"}, {output_frame_size, "640x360"}], Fun).
% @deprecated use convert/4 with <tt>html5ogg</tt> preset
to_html5_ogg(Input, Output) ->
  to_html5_ogg(Input, Output, undefined).
% @deprecated use convert/4 with <tt>html5ogg</tt> preset
to_html5_ogg(Input, Output, Fun) ->
  convert(Input, Output, [{vcodec, "libtheora"}, {output_acodec, "libvorbis"}, {output_frame_size, "640x360"}], Fun).
