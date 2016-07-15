-module(jorel).
-include("../include/jorel.hrl").

-export([main/1]).
-export([version/0]).

main(Args) ->
  State = get_state(Args),
  {providers, Providers} = jorel_config:get(State, providers, []),
  {State1, Providers1} = lists:foldl(fun(Provider, {S, P}) ->
                                         add_provider(S, P, Provider)
                                     end, {State, Providers}, [jorel_provider_release,
                                                               jorel_provider_relup,
                                                               jorel_provider_appup,
                                                               jorel_provider_archive,
                                                               jorel_provider_config,
                                                               jorel_provider_register,
                                                               jorel_provider_publish,
                                                               jorel_provider_install]),
  {paths, Paths} = jorel_config:get(State, paths, []),
  _ = load_paths(Paths),
  State2 = lists:foldl(fun(P, S) ->
                           load_provider(P, S)
                       end, State1, Providers1),
  Commands = get_commands(Args, State2),
  ?DEBUG("* Commands = ~p", [Commands]),
  Options = options(Args, Commands, State2),
  ?DEBUG("* Options = ~p", [Options]),
  case lists:member(help, Options) of
    true ->
      help(Commands, State2);
    false ->
      case lists:member(version, Options) of
        true ->
          version();
        false ->
          case Commands of
            [] ->
              help([], State2);
            _ ->
              State3 = buclists:merge_keylists(1, Options, State2),
              ?DEBUG("* State = ~p", [State3]),
              [try
                 jorel_provider:run(State3, Command)
               catch
                 Error:Reason ->
                   ?DEBUG("Provider ~s failed : ~p:~p", [Command, Error, Reason]),
                   ?DEBUG("================================================", []),
                   ?DEBUG("~p", [erlang:get_stacktrace()]),
                   ?DEBUG("================================================", []),
                   ?HALT("Provider ~s failed!", [Command])
               end || Command <- Commands]
          end
      end
  end.

get_state(Args) ->
  lists:map(fun
              (E) when is_tuple(E) -> E;
              (E) -> {E, true}
            end, read_config(Args)).

read_config(Args) ->
  ConfigFile = find_config_file(Args),
  case filelib:is_file(ConfigFile) of
    true ->
      case file:consult(ConfigFile) of
        {ok, Config} ->
          Config;
        {error, Reason} ->
          ?DEBUG("= Can't read configuration file (~s) : ~p", [ConfigFile, Reason]),
          []
      end;
    false ->
      ?DEBUG("= Configuration file (~s) not found", [ConfigFile]),
      []
  end.

find_config_file([]) ->
  ?CONFIG_FILE;
find_config_file(["--config", Config|_]) ->
  Config;
find_config_file(["-c", Config|_]) ->
  Config;
find_config_file([_|Rest]) ->
  find_config_file(Rest).

get_commands(Args, State) ->
  {providers_def, Providers} = jorel_config:get(State, providers_def),
  ProvidersNames = [P || {P, _} <- Providers],
  buclists:delete_if(fun(E) ->
                         not lists:member(E, ProvidersNames)
                     end, get_commands0(Args, [])).
get_commands0([], Acc) ->
  lists:reverse(Acc);
get_commands0([[$-|_]|_], Acc) ->
  lists:reverse(Acc);
get_commands0([Cmd|Rest], Acc) ->
  get_commands0(Rest, [bucs:to_atom(Cmd)|Acc]).

options(Args, Commands, State) ->
  Opts = commands_options(Commands, State),
  case getopt:parse(Opts, Args) of
    {ok, {Options, _}} ->
      Options;
    {error, {invalid_option, Option}} ->
      ?ERROR("!!! Invalid option ~s", [Option]),
      help(Commands, State);
    {error, {missing_option_arg, Option}} ->
      ?ERROR("!!! Missing argument for option --~s", [Option]),
      help(Commands, State)
  end.

commands_options(Commands, State) ->
  {providers_def, Providers} = jorel_config:get(State, providers_def),
  lists:foldl(fun(Provider, Acc) ->
                  case lists:keyfind(Provider, 1, Providers) of
                    {Provider, #{opts := ProviderOpts}} ->
                      Acc ++ ProviderOpts;
                    _ ->
                      Acc
                  end
              end, opts(), Commands).

opts() ->
  [
   {config,       $c,        "config",       {string, "jorel.config"}, "Path to the config file"},
   {help,         $h,        "help",         undefined,                "Display this help"},
   {version,      $V,        "version",      undefined,                "Display version"}
  ].

help(Commands, State) ->
  Opts = commands_options(Commands, State),
  getopt:usage(Opts, "jorel [commands ...]", ""),
  {providers_def, Providers} = jorel_config:get(State, providers_def),
  io:format(standard_error, "Commands:~n~n", []),
  providers(Commands, Providers),
  halt(0).

providers(Commands, Providers) ->
  lists:foreach(fun({Name, #{depends := Deps, desc := Desc}}) ->
                    case Commands == [] orelse lists:member(Name, Commands) of
                      true ->
                        case Deps of
                          [] ->
                            io:format(standard_error, "  ~s:~n      ~s~n", [Name, Desc]);
                          Deps ->
                            io:format(standard_error, "  ~s:~n      ~s (dependencies: ~p)~n", [Name, Desc, Deps])
                        end;
                      false ->
                        ok
                    end
                end, Providers).

add_provider(State, Providers, Provider) ->
  case lists:member(Provider, Providers) of
    true ->
      {State, Providers};
    false ->
      {jorel_config:set(State, {providers, [Provider|Providers]}),
       [Provider|Providers]}
  end.

load_provider(Provider, State) ->
  _ = application:load(Provider),
  case code:ensure_loaded(Provider) of
    {module, Module} ->
      case erlang:function_exported(Module, init, 1) of
        true ->
          erlang:apply(Module, init, [State]);
        false ->
          ?DEBUG("Provider ~p does not export init/1. It will not be used.", [Provider]),
          State
      end;
    {error, _} ->
      ?DEBUG("Provider ~p not found. It will not be used.", [Provider]),
      State
  end.

load_paths(Paths) ->
  code:add_pathsa(load_paths(Paths, [])).
load_paths([], Acc) ->
  Acc;
load_paths([Path|Rest], Acc) ->
  Ebins = case filelib:wildcard(Path) of
            [Path] -> 
              Ebin = filename:join([Path, "ebin"]),
              case filelib:is_dir(Ebin) of
                true ->
                  [Ebin];
                false ->
                  []
              end;
            Paths ->
              load_paths(Paths, [])
          end,
  load_paths(Rest, Acc ++ Ebins).

version() ->
  application:load(?MODULE),
  {ok, Vsn} = application:get_key(?MODULE, vsn),
  ?REMARK("~s ~s (Erlang/OTP ~s Erts ~s)", [?MODULE, Vsn, 
                                             erlang:system_info(otp_release), 
                                             erlang:system_info(version)]),
  init:stop().

