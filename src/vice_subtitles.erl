-module(vice_subtitles).

-export([
         parse/1
         , parse_file/1
         , to_string/2
         , to_string/3
         , to_file/2
         , to_file/3
         , to_file/4
        ]).

-type cue() :: #{}.
-type subs() :: [cue()].

% @doc
% Parse a subtitle string
% @end
-spec parse(string()) -> {ok, subs()} | {error, {integer(), integer()}}.
parse(Data) when is_list(Data) ->
  {ok, _, _, Tokens} = vice_prv_subs:tokenize(Data),
  case vice_prv_subs_parser:parse(Tokens) of
    {error, {{Line, Column, _}, vice_prv_subs_parser, _}} ->
      {error, {Line, Column}};
    OK ->
      OK
  end.

% @doc
% Parse a subtitle file
% @end
-spec parse_file(file:filename_all()) -> {ok, subs()} | {error, term()} | {error, {integer(), integer()}}.
parse_file(File) ->
  case file:read_file(File) of
    {ok, <<239, 187, 191, Binary/binary>>} -> % BOM
      parse(bucs:to_string(Binary));
    {ok, <<Binary/binary>>} ->
      parse(bucs:to_string(Binary));
    Error ->
      Error
  end.

% @equiv to_string(Subs, Type, #{})
-spec to_string(Subs :: subs(),
                Type :: webvtt | srt) -> {ok, string(), integer(), float()} | no_data.
to_string(Subs, Type) ->
  to_string(Subs, Type, #{}).

% @doc
% Generate a subtitle string
%
% Options:
% <ul>
% <li><tt>from :: string() | binary() | {integer(), integer(), integer(), integer()}</tt> : Starting cue (default: <tt>"00:00:00"</tt>))</li>
% <li><tt>to :: string() | binary() | {integer(), integer(), integer(), integer()} | undefined</tt> : Starting cue (default: <tt>undefined</tt>)</li>
% <li><tt>duration :: integer() | undefined</tt> : Total duration in seconds (default: <tt>undefined</tt>)</li>
% </ul>
%
% <tt>from</tt> and <tt>to</tt> formats :
% <ul>
% <li><tt>HH:MM:SS</tt></li>
% <li><tt>HH:MM:SS.mmm</tt></li>
% <li><tt>{MM, MM, SS, mmm}</tt></li>
% </ul>
% @end
-spec to_string(Subs :: subs(),
                Type :: srt | webvtt,
                Options :: map() | list()) -> {ok, string(), integer(), float(), cue()} | no_data.
to_string(Subs, Type, Options) ->
  vice_prv_subs_writer:to_string(Subs, Type, Options).

% @equiv to_file(Subs, File, #{})
-spec to_file(Subs :: subs(),
              File :: file:filename_all()) -> ok | {error, term()}.
to_file(Subs, File) ->
  to_file(Subs, File, #{}).

% @doc
% Generate a subtitle file.
%
% This function will use the file extension to determine the subtitle format.
%
% M3U8 Options:
% <ul>
% <li><tt>segment_time :: integer()</tt> : Segment duration in seconds (default: 10)</li>
% <li><tt>segment_filename :: string()</tt> : Segments file name (default: <tt>subtitle_%d.vtt</tt>)</li>
% <li><tt>segment_repeat_cue :: true | false</tt> : Repeat the last cue (default: <tt>false</tt>)</li>
% <li>
%   <tt>x_timestamp_map :: {cue_time(), mpeg2_time()}</tt> : Add X-TIMESTAMP-MAP to  maps WebVTT cue timestamps to MPEG-2 (PES) timestamps.
%   <ul>
%     <li><tt>cue_time()</tt> :: <tt>00:00:00.000</tt></li>
%     <li><tt>mpeg2_time()</tt> :: <tt>integer()</tt></li>
%   </ul>
% </li>
% </ul>
%
%
% SRT and WEBVTT options:
% <ul>
% <li><tt>from :: string() | binary() | {integer(), integer(), integer(), integer()}</tt> : Starting cue (default: <tt>"00:00:00"</tt>))</li>
% <li><tt>to :: string() | binary() | {integer(), integer(), integer(), integer()} | undefined</tt> : Starting cue (default: <tt>undefined</tt>)</li>
% <li><tt>duration :: integer() | undefined</tt> : Total duration in seconds (default: <tt>undefined</tt>)</li>
% </ul>
%
% <tt>from</tt> and <tt>to</tt> formats :
% <ul>
% <li><tt>HH:MM:SS</tt></li>
% <li><tt>HH:MM:SS.mmm</tt></li>
% <li><tt>{MM, MM, SS, mmm}</tt></li>
% </ul>
% @end
-spec to_file(Subs :: subs(),
              File :: file:filename_all(),
              Options :: map() | list()) -> ok | no_data | {error, term()}.
to_file(Subs, File, Options) ->
  case type(filename:extension(File)) of
    {ok, Type} ->
      to_file(Subs, File, Options, Type);
    Error ->
      Error
  end.

type(".srt") -> {ok, str};
type(".webvtt") -> {ok, webvtt};
type(".vtt") -> {ok, webvtt};
type(".m3u8") -> {ok, m3u8};
type(Other) when is_list(Other) -> {error, invalid_type};
type(Other) -> type(bucs:to_string(Other)).

% @doc
% Generate a subtitle file in the given format.
%
% M3U8 Options:
% <ul>
% <li><tt>segment_time :: integer()</tt> : Segment duration in seconds (default: 10)</li>
% <li><tt>segment_filename :: string()</tt> : Segments file name (default: <tt>subtitle_%d.vtt</tt>)</li>
% <li><tt>segment_repeat_cue :: true | false</tt> : Repeat the last cue (default: <tt>false</tt>)</li>
% <li>
%   <tt>x_timestamp_map :: {cue_time(), mpeg2_time()}</tt> : Add X-TIMESTAMP-MAP to  maps WebVTT cue timestamps to MPEG-2 (PES) timestamps.
%   <ul>
%     <li><tt>cue_time()</tt> :: <tt>00:00:00.000</tt></li>
%     <li><tt>mpeg2_time()</tt> :: <tt>integer()</tt></li>
%   </ul>
% </li>
% </ul>
%
% SRT and WEBVTT options:
% <ul>
% <li><tt>from :: string() | binary() | {integer(), integer(), integer(), integer()}</tt> : Starting cue (default: <tt>"00:00:00"</tt>))</li>
% <li><tt>to :: string() | binary() | {integer(), integer(), integer(), integer()} | undefined</tt> : Starting cue (default: <tt>undefined</tt>)</li>
% <li><tt>duration :: integer() | undefined</tt> : Total duration in seconds (default: <tt>undefined</tt>)</li>
% </ul>
%
% <tt>from</tt> and <tt>to</tt> formats :
% <ul>
% <li><tt>HH:MM:SS</tt></li>
% <li><tt>HH:MM:SS.mmm</tt></li>
% <li><tt>{MM, MM, SS, mmm}</tt></li>
% </ul>
% @end
-spec to_file(Subs :: subs(),
              File :: file:filename_all(),
              Options :: map() | list(),
              Type :: webvtt | srt | m3u8) -> ok | no_data | {error, term()}.
to_file(Subs, File, Options, Type) ->
  vice_prv_subs_writer:to_file(Subs, Type, File, Options).
