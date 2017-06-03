%%% -*- mode: erlang -*-

Header
  "% @hidden".

Nonterminals
  Start
  CueIdentifier
  CueTimings CueTiming
  Newlines
  Timing Time
  Text.

Terminals
  newline space
  arrow colon period comma
  digit string.

Rootsymbol
  Start.

Start -> CueTimings : '$1'.

CueTimings -> CueIdentifier CueTiming : [add_identifier('$1', '$2')].
CueTimings -> CueIdentifier CueTiming Newlines : [add_identifier('$1', '$2')].
CueTimings -> CueIdentifier CueTiming Newlines CueTimings : [add_identifier('$1', '$2')|'$4'].
CueTimings -> CueTiming : ['$1'].
CueTimings -> CueTiming Newlines : ['$1'].
CueTimings -> CueTiming Newlines CueTimings : ['$1'|'$3'].

CueIdentifier -> digit newline : digit('$1').

Newlines -> Newlines newline : none.
Newlines -> newline : none.

CueTiming -> Timing Text : #{duration => '$1', text => '$2'}.

Timing -> Time space arrow space Time newline : #{from => '$1', to => '$5'}.

Time -> digit colon digit colon digit period digit : #{hh => digit('$1'),
                                                       mm => digit('$3'),
                                                       ss => digit('$5'),
                                                       ex => digit('$7')}.
Time -> digit colon digit colon digit comma digit : #{hh => digit('$1'),
                                                      mm => digit('$3'),
                                                      ss => digit('$5'),
                                                      ex => digit('$7')}.

Text -> string newline Text : string('$1') ++ "\n" ++ '$3'.
Text -> string newline : string('$1') ++ "\n".
Text -> string : string('$1').

Erlang code.

add_identifier(Identifier, CueTiming) ->
  CueTiming#{identifier => Identifier}.

digit({digit, _, D}) -> D.
string({string, _, S}) -> S.

