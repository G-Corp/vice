%%% -*- mode: erlang -*-
%%% https://w3c.github.io/webvtt/

Header
  "% @hidden".

Nonterminals
  Start
  CueIdentifier
  Note
  Style
  Region
  CueBlocks CueBlock
  CodecPrivate
  NSRBlocks
  Newlines
  Timing Time
  Text StyleText.

Terminals
  note
  style
  region
  newline space
  arrow colon period comma
  digit string.

Rootsymbol
  Start.

Start -> CueBlocks : #{cues => '$1'}.
Start -> CodecPrivate CueBlocks : add_codec_private('$1', #{cues => '$2'}).

CueBlocks -> CueBlock : ['$1'].
CueBlocks -> CueBlock CueBlocks : ['$1'|'$2'].

CueBlock -> Newlines CueBlock : '$2'.
CueBlock -> Timing Text : #{duration => '$1', text => '$2'}.
CueBlock -> Timing Text Note : #{duration => '$1', text => '$2', note => '$3'}.
CueBlock -> CueIdentifier Timing Text : #{identifier => '$1', duration => '$2', text => '$3'}.
CueBlock -> CueIdentifier Timing Text Note : #{identifier => '$1', duration => '$2', text => '$3', note => '$4'}.

CueIdentifier -> digit newline : digit('$1').
CueIdentifier -> digit space newline : digit('$1').

Timing -> Time space arrow space Time newline : #{from => '$1', to => '$5', id => subid('$1'), length => sublength('$1', '$5'), duration => duration('$1', '$5')}.
Timing -> Time space arrow space Time space newline : #{from => '$1', to => '$5', id => subid('$1'), length => sublength('$1', '$5'), duration => duration('$1', '$5')}.
Timing -> Time space arrow space Time space string newline : #{from => '$1', to => '$5', id => subid('$1'), length => sublength('$1', '$5'), duration => duration('$1', '$5'), settings => cue_settings('$7')}.

Time -> digit colon digit colon digit period digit : #{hh => digit('$1'),
                                                       mm => digit('$3'),
                                                       ss => digit('$5'),
                                                       ex => digit('$7')}.
Time -> digit colon digit colon digit comma digit : #{hh => digit('$1'),
                                                      mm => digit('$3'),
                                                      ss => digit('$5'),
                                                      ex => digit('$7')}.

CodecPrivate -> string Newlines : is_webvtt('$1').
CodecPrivate -> string newline NSRBlocks : maps:merge(is_webvtt('$1'), '$3').

NSRBlocks -> Note : #{notes => ['$1']}.
NSRBlocks -> Note NSRBlocks : add_note('$1', '$2').
NSRBlocks -> Region : #{regions => ['$1']}.
NSRBlocks -> Region NSRBlocks : add_region('$1', '$2').
NSRBlocks -> Style : #{styles => ['$1']}.
NSRBlocks -> Style NSRBlocks : add_style('$1', '$2').

Note -> Newlines Note : '$2'.
Note -> note space string : string('$3').
Note -> note space string Newlines : string('$3').
Note -> note newline Text : '$3'.
Note -> note space newline Text : '$4'.

Style -> Newlines Style : '$2'.
Style -> style newline StyleText : '$3'.
Style -> style space newline StyleText : '$4'.

Region -> Newlines Region : '$2'.
Region -> region newline Text : '$3'.
Region -> region space newline Text : '$4'.

Text -> string newline Text : string('$1') ++ "\n" ++ '$3'.
%Text -> digit Text : digit('$1') ++ '$2'.
%Text -> digit space Text : digit('$1') ++ " " ++ '$2'.
Text -> string Newlines : string('$1').
Text -> string : string('$1').
Text -> period Text : "." ++ '$2'.
Text -> space Text : " " ++ '$2'.

StyleText -> colon colon string newline Text : "::" ++ string('$3') ++ "\n" ++ '$5'.
StyleText -> colon colon string newline Text StyleText : "::" ++ string('$3') ++ "\n" ++ '$5' ++ "\n" ++ '$6'.
StyleText -> colon colon string : "::" ++ string('$1').
StyleText -> Text : '$1'.

Newlines -> space newline Newlines : '$3' + 1.
Newlines -> newline Newlines : '$2' + 1.
Newlines -> space newline : 1.
Newlines -> newline : 1.

% Erlang

Erlang code.

add_codec_private(CodecPrivate, Cues) ->
  case maps:size(CodecPrivate) of
    0 -> Cues;
    _ -> Cues#{codec_private => CodecPrivate}
  end.

subid(#{hh := HH, mm := MM, ss := SS, ex := MS}) ->
  (((bucs:to_integer(HH) * 60 * 60) +
    (bucs:to_integer(MM) * 60) +
    bucs:to_integer(SS)) * 1000) +
  bucs:to_integer(MS).

sublength(Start, End) ->
  subid(End) - subid(Start).

duration(Start, End) ->
  sublength(Start, End) / 1000.

is_webvtt({string, _, [$W, $E, $B, $V, $T, $T|Rest]}) ->
  #{webvtt => string:strip(string:strip(Rest, left, 32), left, 9)}.

add_note(Note, #{notes := Notes} = NSRBlocks) ->
  NSRBlocks#{notes => [Note|Notes]};
add_note(Note, NSRBlocks) ->
  NSRBlocks#{notes => [Note]}.

add_style(Style, #{styles := Styles} = NSRBlocks) ->
  NSRBlocks#{styles => [Style|Styles]};
add_style(Style, NSRBlocks) ->
  NSRBlocks#{styles => [Style]}.

add_region(Region, #{regions := Regions} = NSRBlocks) ->
  NSRBlocks#{regions => [Region|Regions]};
add_region(Region, NSRBlocks) ->
  NSRBlocks#{regions => [Region]}.

cue_settings({string, _, Settings}) ->
  [case [K, V] = string:tokens(X, ":") of
     [K, V] -> {K, V};
     _ -> X
   end || X <- string:tokens(Settings, " \t")].

digit({digit, _, D}) -> D.
string({string, _, S}) -> S.

