-module(vice_subtitles).

-export([parse/1, parse_file/1, to_string/2, to_file/2, to_file/3]).

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

% @doc
% Generate a subtitle string
% @end
-spec to_string(subs(), srt | webvtt) -> {ok, string()} | {error, term()}.
to_string(_Subs, _Type) ->
  todo.

% @doc
% Generate a subtitle file.
%
% This function will use the file extension to determine the subtitle format.
% @end
to_file(Subs, File) ->
  case filename:extension(File) of
    ".srt" ->
      to_file(Subs, srt, File);
    ".webvtt" ->
      to_file(Subs, webvtt, File);
    ".vtt" ->
      to_file(Subs, webvtt, File);
    _ ->
      {error, invalid_type}
  end.

% @doc
% Generate a subtitle file in the given format.
% @end
-spec to_file(subs(), srt | webvtt, file:filename_all()) -> ok | {error, term()}.
to_file(_Subs, _Type, _File) ->
  todo.

