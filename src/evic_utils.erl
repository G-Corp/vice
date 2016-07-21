% @hidden
-module(evic_utils).

-export([
         find_executable/2
         , to_hms/1
        ]).

-define(DEC(X), $0 + X div 10, $0 + X rem 10).

find_executable([], Alternative) ->
  case doteki:get_env(Alternative, undefined) of
    undefined ->
      undefined;
    AppPath ->
      bucs:to_string(AppPath)
  end;
find_executable([App|Rest] = AppList, Alternative) ->
  case bucs:is_string(AppList) of
    true ->
      find_executable([AppList], Alternative);
    false ->
      case os:find_executable(bucs:to_string(App)) of
        false ->
          find_executable(Rest, Alternative);
        AppPath ->
          AppPath
      end
  end.

to_hms(Duration) ->
  SS = trunc(Duration),
  HH = trunc(SS / 3600),
  SS1 = SS - (HH * 3600),
  MM = trunc(SS1 / 60),
  SS2 = SS1 - (MM * 60),
  [?DEC(HH), $:, ?DEC(MM), $:, ?DEC(SS2)].

