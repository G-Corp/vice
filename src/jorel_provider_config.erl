% @hidden
-module(jorel_provider_config).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).

-define(PROVIDER, gen_config).
-define(EXCLUDE, ["**/_jorel/**", "**/_rel*/**", "**/test/**"]).

init(State) ->
  jorel_config:add_provider(
    State,
    {?PROVIDER,
     #{
        module => ?MODULE
        , depends => []
        , desc => "Create a configuration file"
        , opts => [
                   {relname, $n, "relname", string, "Specify the name for the release that will be generated"},
                   {relvsn,  $v, "relvsn",  string, "Specify the version for the release"}
                  ]
      }
    }
   ).

do(State) ->
  ?INFO("== Start provider ~p", [?PROVIDER]),
  ?INFO("= Elixir project: ~p", [jorel_elixir:exist()]),
  RelName = case lists:keyfind(relname, 1, State) of
              {relname, R} -> 
                bucs:to_atom(R);
              _ -> 
                case file:get_cwd() of
                  {ok, D} -> 
                    case filename:basename(D) of
                      [] -> noname;
                      X -> bucs:to_atom(X)
                    end;
                  _ -> 
                    noname
                end
            end,
  ExApp = case jorel_elixir:exist() of
            true ->
              [elixir, 
               bucs:to_atom(
                 string:strip(
                   jorel_cmd:run(
                     jorel_elixir:iex() ++ " --erl \"+A0\" -e " ++
                     "'Application.ensure_all_started(:mix); " ++
                     "Code.eval_file(\"mix.exs\"); " ++
                     "IO.puts(Mix.Project.config[:app]); " ++ 
                     "System.halt()' | tail -1"), right, 10))];
            false ->
              []
          end,
  RelVsn = case lists:keyfind(relvsn, 1, State) of
             {relvsn, V} -> V;
             _ -> "1.0.0"
           end,
  Output = case lists:keyfind(config, 1, State) of
            {config, C} -> C;
            _ -> "jorel.config"
          end,
  BootApps = lists:usort(
               lists:foldl(fun(App, Acc) ->
                               [bucs:to_atom(filename:basename(App, ".app"))|Acc]
                           end, [sasl], 
                           ExApp ++ filelib:wildcard("ebin/*.app") ++ filelib:wildcard("apps/*/ebin/*.app"))),
  AllApps = lists:usort(
              lists:foldl(fun(App, Acc) ->
                              [bucs:to_atom(filename:basename(App, ".app"))|Acc]
                          end, [sasl], 
                          ExApp ++ bucfile:wildcard("**/ebin/*.app", ?EXCLUDE, [expand_path]))),
  case erlconf:open('jorel.config', Output, [{save_on_close, false}]) of
    {ok, _} ->
      ?INFO("* Create file ~s", [Output]),
      Term = [
              {release, {RelName, RelVsn}, AllApps},
              {boot, BootApps},
              {all_deps, false},
              {output_dir, "_jorel"},
              {exclude_dirs, ?EXCLUDE},
              {include_src, false},
              {include_erts, true},
              {disable_relup, false},
              {providers, [jorel_provider_tar, jorel_provider_zip, jorel_provider_deb, jorel_provider_git_tag]}
             ],
      Term1 = case filelib:wildcard("config/*.config") of
                [Config] -> 
                  Term ++ [{sys_config, Config}];
                _ ->
                  case jorel_elixir:exist() andalso filelib:is_file("config/config.exs") of
                    true -> Term ++ [{sys_config, "config/config.exs"}];
                    _ -> Term
                  end
              end,
      Term2 = case filelib:is_regular("config/vm.args") of
                true ->
                  Term1 ++ [{vm_args, "config/vm.args"}];
                _ ->
                  Term1
              end,
      {ok, Term2} = erlconf:term('jorel.config', Term2),
      ok = erlconf:save('jorel.config'),
      close = erlconf:close('jorel.config');
    E ->
      ?HALT("!!! Can't create file ~s: ~p", [Output, E])
  end,
  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State.

