% @hidden
-module(vice_prv_ffmpeg).
-compile([{parse_transform, lager_transform}]).

-export([
         init/0,
         infos/2,
         info/3,
         convert/6
        ]).

-record(state, {
          prober,
          converter
         }).

-define(PROBE, "~s -v quiet -of json -show_format -show_streams \"~ts\"").

init() ->
  case vice_utils:find_executable(["ffprobe"], [vice, ffmpeg, ffprobe]) of
    undefined ->
      {stop, {ffprobe, not_found}};
    FFProbe ->
      case vice_utils:find_executable(["ffmpeg"], [vice, ffmpeg, ffmpeg]) of
        undefined ->
          {stop, {ffmpeg, not_found}};
        FFMpeg ->
          {ok, #state{
                  prober = FFProbe,
                  converter = FFMpeg
                 }}
      end
  end.

infos(#state{prober = Prober}, File) ->
  Cmd = lists:flatten(io_lib:format(?PROBE, [Prober, File])),
  case bucos:run(Cmd) of
    {ok, Output} ->
      {ok, jsx:decode(bucs:to_binary(Output), [{labels, atom}, return_maps])};
    Error ->
      Error
  end.

info(State, File, Info) ->
  case infos(State, File) of
    {ok, Infos} ->
      get_info(Infos, Info);
    Error ->
      Error
  end.

get_info(#{format := #{duration := Duration}}, duration) ->
  {ok, bucs:to_float(Duration)};
get_info(_, _) ->
  {error, unavailable}.


convert(#state{converter = Converter}, In, Out, Options, Fun, From) ->
  case Fun of
    sync ->
      ok;
    _ ->
      gen_server:reply(From, {async, self()})
  end,
  Cmd = gen_command(Converter, In, Out, Options, [{yes, true}], []),
  lager:info("COMMAND : ~p", [Cmd]),
  case bucos:run(Cmd) of
    {ok, _} ->
      vice_utils:reply(Fun, From, {ok, In, Out});
    Error ->
      vice_utils:reply(Fun, From, Error)
  end,
  gen_server:cast(vice, {terminate, self()}).

gen_command(Converter, In, Out, Options, OverwriteOptions, MissingOptions) ->
  Options1 = buclists:merge_keylists(1, OverwriteOptions, Options),
  Options2 = buclists:merge_keylists(1, MissingOptions, Options1),
  gen_options(Converter, In, Out, Options2).

gen_options(Converter, In, Out, Options) ->
  [
    {input, InputOptions},
    {output, OutputOptions},
    {global, GlobalOptions}
  ] = vice_prv_ffmpeg_options:options(Options),
  lists:flatten(
    io_lib:format(
      "~s~s~s -i \"~ts\"~s \"~ts\"",
      [Converter, GlobalOptions, InputOptions, In, OutputOptions, Out])).

