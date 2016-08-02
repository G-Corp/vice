% @hidden
-module(jorel_config).
-include("../include/jorel.hrl").

-export([
         add_provider/2,
         set/2,
         get/2,
         get/3
        ]).

add_provider(State, Provider) ->
  {providers_def, Providers} = get(State, providers_def, []),
  set(State, {providers_def, [Provider|Providers]}).

set(State, {Key, _} = Data) ->
  State2 = lists:keyreplace(Key, 1, State, Data),
  case lists:keyfind(Key, 1, State2) of
    {Key, _} -> State2;
    false -> [Data|State2]
  end.

get(State, outdir) ->
  {output_dir, Outdir} = get(State, output_dir),
  {relname, RelName} = get(State, relname),
  {outdir, filename:join(Outdir, RelName)};

get(State, relname) ->
  case get(State, relname, undefined) of
    {relname, undefined} ->
      {release, {RelName, _}, _} = get(State, release),
      {relname, RelName};
    RelName1 -> RelName1
  end;

get(State, relvsn) ->
  case get(State, relvsn, undefined) of
    {relvsn, undefined} ->
      {release, {_, RelVsn}, _} = get(State, release),
      {relvsn, RelVsn};
    RelVsn1 -> RelVsn1
  end;

get(State, binfile) ->
  {outdir, Outdir} = get(State, outdir),
  {relname, RelName} = get(State, relname),
  {binfile, filename:join([Outdir, "bin", RelName])};

get(State, Key) ->
  case lists:keyfind(Key, 1, State) of
    false ->
      ?HALT("Missing configuration value for ~s", [Key]);
    T -> T
  end.
get(State, Key, Default) ->
  case lists:keyfind(Key, 1, State) of
    false ->
      {Key, Default};
    T -> T
  end.

