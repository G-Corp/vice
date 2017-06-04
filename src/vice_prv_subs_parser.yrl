%%% -*- mode: erlang -*-

Header
  "% @hidden".

Nonterminals
  Start
  CueIdentifier
  Note
  Style
  Region
  CueTimings CueTiming
  CodecPrivate
  NSR
  Newlines
  Timing Time
  Text ContinousText StyleText.

Terminals
  note
  style
  region
  newline space
  arrow colon period comma
  digit string.

Rootsymbol
  Start.

Start -> CueTimings : #{cues => '$1'}.
Start -> CodecPrivate CueTimings : add_codec_private('$1', #{cues => '$2'}).

CueTimings -> CueIdentifier CueTiming : [cue_identifier('$1', '$2')].
CueTimings -> CueIdentifier CueTiming CueTimings : [cue_identifier('$1', '$2')|'$3'].
CueTimings -> CueTiming : ['$1'].
CueTimings -> CueTiming CueTimings : ['$1'|'$2'].

CodecPrivate -> Newlines : #{}.
CodecPrivate -> string Newlines : is_webvtt('$1').
CodecPrivate -> string Newlines NSR : maps:merge(is_webvtt('$1'), '$3').
CodecPrivate -> Newlines string Newlines : is_webvtt('$2').
CodecPrivate -> Newlines string Newlines NSR : maps:merge(is_webvtt('$2'), '$4').
CodecPrivate -> Newlines string Newlines NSR Newlines: maps:merge(is_webvtt('$2'), '$4').

NSR -> Note NSR : add_note('$1', '$2').
NSR -> Note : #{notes => ['$1']}.
NSR -> Region NSR : add_region('$1', '$2').
NSR -> Region : #{regions => ['$1']}.
NSR -> Style NSR : add_style('$1', '$2').
NSR -> Style : #{styles => ['$1']}.

Note -> note space string : string('$3').
Note -> note space string Newlines : string('$3').
Note -> note newline ContinousText : '$3'.

Style -> style newline StyleText : '$3'.

Region -> region newline ContinousText : '$3'.

CueIdentifier -> digit newline : digit('$1').

CueTiming -> Timing Text : #{duration => '$1', text => '$2'}.
CueTiming -> Timing Text Note : #{duration => '$1', text => '$2', note => '$3'}.
CueTiming -> Timing Text Note Newlines : #{duration => '$1', text => '$2', note => '$3'}.

Timing -> Time space arrow space Time space string newline : #{from => '$1', to => '$5', settings => cue_settings('$7')}.
Timing -> Time space arrow space Time newline : #{from => '$1', to => '$5'}.

Time -> digit colon digit colon digit period digit : #{hh => digit('$1'),
                                                       mm => digit('$3'),
                                                       ss => digit('$5'),
                                                       ex => digit('$7')}.
Time -> digit colon digit colon digit comma digit : #{hh => digit('$1'),
                                                      mm => digit('$3'),
                                                      ss => digit('$5'),
                                                      ex => digit('$7')}.

Text -> string Newlines Text : string('$1') ++ newlines('$2') ++ '$3'.
Text -> string Newlines : string('$1') ++ "\n".
Text -> string : string('$1').

ContinousText -> string newline ContinousText : string('$1') ++ "\n" ++ '$3'.
ContinousText -> string Newlines : string('$1').
ContinousText -> string : string('$1').

StyleText -> colon colon string newline ContinousText : "::" ++ string('$3') ++ "\n" ++ '$5'.
StyleText -> colon colon string newline ContinousText StyleText : "::" ++ string('$3') ++ "\n" ++ '$5' ++ "\n" ++ '$6'.
StyleText -> colon colon string Newlines : "::" ++ string('$1').
StyleText -> colon colon string : "::" ++ string('$1').
StyleText -> ContinousText : '$1'.

Newlines -> Newlines newline : '$1' + 1.
Newlines -> newline : 1.

Erlang code.

add_codec_private(CodecPrivate, Cues) ->
  case maps:size(CodecPrivate) of
    0 -> Cues;
    _ -> Cues#{codec_private => CodecPrivate}
  end.

is_webvtt({string, _, [$W, $E, $B, $V, $T, $T|Rest]}) ->
  #{webvtt => string:strip(string:strip(Rest, left, 32), left, 9)}.

add_note(Note, #{notes := Notes} = NSR) ->
  NSR#{notes => [Note|Notes]};
add_note(Note, NSR) ->
  NSR#{notes => [Note]}.

add_style(Style, #{styles := Styles} = NSR) ->
  NSR#{styles => [Style|Styles]};
add_style(Style, NSR) ->
  NSR#{styles => [Style]}.

add_region(Region, #{regions := Regions} = NSR) ->
  NSR#{regions => [Region|Regions]};
add_region(Region, NSR) ->
  NSR#{regions => [Region]}.

cue_settings({string, _, Settings}) ->
  [case [K, V] = string:tokens(X, ":") of
     [K, V] -> {K, V};
     _ -> X
   end || X <- string:tokens(Settings, " \t")].

cue_identifier(Identifier, CueTiming) ->
  CueTiming#{identifier => Identifier}.

digit({digit, _, D}) -> D.
string({string, _, S}) -> S.

newlines(N) ->
  lists:flatten(lists:duplicate(N, "\n")).

