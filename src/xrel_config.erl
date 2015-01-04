-module(xrel_config).
-include("../include/xrel.hrl").

-export([
         to_state/1,
         get/2,
         get/3
        ]).

to_state(Options) ->
  State = case lists:keyfind(config, 1, Options) of
            {config, ConfigFile} ->
              case filelib:is_file(ConfigFile) of
                true -> 
                  elists:merge_keylists(
                    1, read_config(ConfigFile), Options);
                false ->
                  ?HALT("File ~s not found", [ConfigFile])
              end;
            _ ->
              ?HALT("Missing config file", [])
          end,
  lists:map(fun
              (E) when is_tuple(E) -> E;
              (E) -> {E, true}
            end, State).

get(State, outdir) ->
  {output_dir, Outdir} = get(State, output_dir),
  {relname, RelName} = get(State, relname),
  {outdir, filename:join(Outdir, RelName)};

get(State, relname) ->
  {release, {RelName, _}, _} = get(State, release),
  case get(State, relname, undefined) of
    {relname, undefined} -> {relname, RelName};
    RelName1 -> RelName1
  end;

get(State, relvsn) ->
  {release, {_, RelVsn}, _} = get(State, release),
  case get(State, relvsn, undefined) of
    {relvsn, undefined} -> {relvsn, RelVsn};
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

% Private
  
read_config(File) ->
  ?INFO("* Read configuration from ~s", [File]),
  case file:consult(File) of
    {ok, Config} -> 
      Config;
    {error, Reason} ->
      ?HALT("Error while reading ~s: ~p", [File, Reason])
  end.
