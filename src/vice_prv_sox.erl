% @hidden
-module(vice_prv_sox).
-compile([{parse_transform, lager_transform}]).
-include_lib("bucs/include/bucs.hrl").
-include("../include/vice_sox.hrl").
-behaviour(vice_encoder).

%% API
-export([
         init/0
         , infos/3
         , info/4
         , command/5
         , progress/2
        ]).

-record(state, {
          sox,
          soxi
         }).

-define(INFO_FORMAT, "~s ~s \"~ts\"").
-define(INFOS, #{
          sample_rate => {"-r", fun bucs:to_integer/1},
          number_of_channels => {"-c", fun bucs:to_integer/1},
          number_of_samples => {"-s", fun bucs:to_integer/1},
          duration => {"-d", fun bucs:to_binary/1},
          seconds_duration => {"-D", fun bucs:to_float/1},
          bits_per_sample => {"-b", fun bucs:to_integer/1},
          bitrate => {"-B", fun(Value) ->
                                bucs:to_integer(re:replace(Value, "[^0-9]*$", "", [{return, list}]))
                            end},
          bitrate_unit => {"-B", fun(Value) ->
                                     re:replace(Value, "^[0-9]*", "", [{return, binary}])
                                 end},
          encoding => {"-e", fun bucs:to_binary/1},
          annotations => {"-a", fun bucs:to_binary/1}
         }).

init() ->
  case vice_utils:find_tools(record_info(fields, state), sox) of
    {error, Reason} ->
      {stop, Reason};
    {state, Data} ->
      {ok, ?list_to_record(state, Data)}
  end.

infos(State, File, Options) ->
  Labels = buclists:keyfind(labels, 1, Options, atom),
  {ok, maps:fold(fun(Info, _, Acc) ->
                     case info(State, File, Info, Options) of
                       {ok, Data} ->
                         case Labels of
                           binary ->
                             maps:put(bucs:to_binary(Info), Data, Acc);
                           _ ->
                             maps:put(Info, Data, Acc)
                         end;
                       _ ->
                         Acc
                     end
                 end, #{}, ?INFOS)}.

info(#state{soxi = Soxi}, File, Info, _Options) ->
  case maps:get(Info, ?INFOS, undefined) of
    undefined ->
      {error, unavailable};
    {Param, ConvertFunction} ->
      Cmd = lists:flatten(io_lib:format(?INFO_FORMAT, [Soxi, Param, File])),
      lager:debug("COMMAND : ~p", [Cmd]),
      case bucos:run(Cmd) of
        {ok, Output} ->
          {ok, erlang:apply(ConvertFunction, [vice_prv_stdlib:chomp(Output)])};
        Error ->
          Error
      end
  end.

command(#state{sox = Converter}, In, Out, Options, _Multi) ->
  Options1 = buclists:merge_keylists(1, [{clobber, true}, {show_progress, true}], Options),
  Params = vice_prv_options:options(?OPTIONS, Options1),
  InputOptions = buclists:keyfind(input, 1, Params, ""),
  OutputOptions = buclists:keyfind(output, 1, Params, ""),
  GlobalOptions = buclists:keyfind(global, 1, Params, ""),
  EffectsOptions = buclists:keyfind(effects, 1, Params, ""),
  Inputs = build_input(In, InputOptions),
  {ok, lists:flatten(
    io_lib:format(
      "~s~s~s~s \"~ts\"~s",
      [Converter, GlobalOptions, Inputs, OutputOptions, Out, EffectsOptions]))}.

build_input([X|_] = Ins, [Y|_] = InputOptions) when is_list(X), is_list(Y) ->
  build_input(Ins, InputOptions, "");
build_input([X|_] = Ins, InputOptions) when is_list(X) ->
  build_input(Ins, [InputOptions]);
build_input(In, [InputOptions|_]) when is_list(InputOptions) ->
  build_input(In, InputOptions);
build_input(In, InputOptions) ->
  lists:flatten(
    io_lib:format(
      "~s \"~ts\"", [InputOptions, In])).

build_input([], _, Result) ->
  Result;
build_input([In|Ins], [Options|InputOptions], Result) ->
  build_input(
    Ins, InputOptions,
    Result ++ " " ++ build_input(In, Options));
build_input([In|Ins], [], Result) ->
  build_input(
    Ins, [],
    Result ++ build_input(In, "")).

progress(_Bytes, Sofar) ->
  Sofar.

% format option

%format(FMT, Args) ->
%  lists:flatten(io_lib:format(FMT, Args)).
