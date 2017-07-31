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

-type subs() :: #{}.

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
                Type :: webvtt | srt) -> {ok, string(), integer()} | no_data.
to_string(Subs, Type) ->
  to_string(Subs, Type, #{}).

% @doc
% Generate a subtitle string
%
% Options:
% <ul>
% <li>from</li>
% <li>to</li>
% <li>duration</li>
% </ul>
% @end
-spec to_string(Subs :: subs(),
                Type :: srt | webvtt,
                Options :: map()) -> {ok, string(), integer()} | no_data.
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
% Options:
% <ul>
% <li>segment_time</li>
% <li>segment_filename</li>
% <li>from</li>
% <li>to</li>
% <li>duration</li>
% </ul>
% @end
-spec to_file(Subs :: subs(),
              File :: file:filename_all(),
              Options :: map()) -> ok | {error, term()}.
to_file(Subs, File, Options) ->
  case filename:extension(File) of
    ".srt" ->
      to_file(Subs, File, Options, srt);
    ".webvtt" ->
      to_file(Subs, File, Options, webvtt);
    ".vtt" ->
      to_file(Subs, File, Options, webvtt);
    ".m3u8" ->
      to_file(Subs, File, Options, m3u8);
    _ ->
      {error, invalid_type}
  end.

% @doc
% Generate a subtitle file in the given format.
%
% Options:
% <ul>
% <li>segment_time</li>
% <li>segment_filename</li>
% <li>from</li>
% <li>to</li>
% <li>duration</li>
% </ul>
% @end
-spec to_file(Subs :: subs(),
              File :: file:filename_all(),
              Options :: map(),
              Type :: webvtt | srt | m3u8) -> ok | {error, term()}.
to_file(Subs, File, Options, Type) ->
  vice_prv_subs_writer:to_file(Subs, Type, File, Options).
