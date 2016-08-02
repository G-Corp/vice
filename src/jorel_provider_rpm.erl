% @hidden
-module(jorel_provider_rpm).
-behaviour(jorel_provider).
-include("../include/jorel.hrl").

-export([init/1, do/1]).
-define(PROVIDER, rpm).

-define(PROJECT_GROUP, "Applications").
-define(PROJECT_LICENCE, "BSD-3").
-define(PROJECT_BUILD_ARCHITECTURE, "x86_64").
-define(PROJECT_SUMMARY, "No summary").
-define(PROJECT_DESCRIPTION, "No description").
-define(PROJECT_URL, "https://github.com/emedia-project/jorel").
-define(PROJECT_AUTHOR_NAME, "<developer name>").
-define(PROJECT_AUTHOR_MAIL, "people@example.com").

init(State) ->
  jorel_config:add_provider(
    State,
    {?PROVIDER,
     #{
       module => ?MODULE,
       depends => [release],
       desc => "Create a RPM package"
      }
    }
   ).

do(State) ->
  ?INFO("== Start provider ~p", [?PROVIDER]),
  {output_dir, Outdir} = jorel_config:get(State, output_dir),
  {relname, RelName} = jorel_config:get(State, relname),
  {relvsn, RelVsn} = jorel_config:get(State, relvsn),
  RPMBuildDir = filename:join([Outdir, "rpmbuild"]),
  {rpm, RPMConfig} = jorel_config:get(State, rpm, []),
  RPMData = [
             {project_topdir, bucfile:expand_path(RPMBuildDir)},
             {relname, RelName},
             {relvsn, bucstring:gsub(RelVsn, "-", ".")},
             {build_date, bucdate:format("D M j Y", bucdate:today())},
             jorel_config:get(RPMConfig, package_summary, ?PROJECT_SUMMARY),
             jorel_config:get(RPMConfig, package_group, ?PROJECT_GROUP),
             jorel_config:get(RPMConfig, license_type, ?PROJECT_LICENCE),
             jorel_config:get(RPMConfig, package_url, ?PROJECT_URL),
             jorel_config:get(RPMConfig, build_architecture, ?PROJECT_BUILD_ARCHITECTURE),
             jorel_config:get(RPMConfig, package_description, ?PROJECT_DESCRIPTION),
             jorel_config:get(RPMConfig, author_name, ?PROJECT_AUTHOR_NAME),
             jorel_config:get(RPMConfig, author_email, ?PROJECT_AUTHOR_MAIL)
            ],
  lists:foreach(fun(Dir) ->
                    Dir1 = filename:join([RPMBuildDir, Dir]),
                    case bucfile:make_dir(Dir1) of
                      ok ->
                        ?INFO("* Create directory ~s", [Dir1]);
                      {error, Reason} ->
                        ?HALT("! Faild to create directory ~s: ~p", [Dir1, Reason])
                    end
                end, [
                      ["SPECS"],
                      ["SOURCES"],
                      ["RPMS"],
                      ["SRPMS"],
                      ["BUILD"]
                     ]),
  SpecFile = filename:join([RPMBuildDir, "SPECS", bucs:to_string(RelName) ++ ".spec"]),
  ?INFO("* Create file ~s", [SpecFile]),
  {ok, SpecContent} = jorel_rpm_spec_dtl:render(RPMData),
  ok = file:write_file(SpecFile, iolist_to_binary(SpecContent)),
  Tar = bucs:to_list(RelName) ++ "-" ++ RelVsn ++ ".tar.gz",
  SourceFile = filename:join([Outdir, RelName, "releases", Tar]),
  case filelib:is_file(SourceFile) of
    true -> ok;
    false -> jorel_provider_tar:do(State)
  end,
  TarRpm = bucs:to_list(RelName) ++ "-" ++ bucstring:gsub(RelVsn, "-", ".") ++ ".tar.gz",
  SourceDest = filename:join([RPMBuildDir, "SOURCES", TarRpm]),
  case file:copy(SourceFile, SourceDest) of
    {ok, _} ->
      ?INFO("* Copy source ~s to ~s", [SourceFile, SourceDest]);
    {error, Reason} ->
      ?HALT("! Faild to copy ~s to ~s: ~p", [SourceFile, SourceDest, Reason])
  end,
  {ok, InitContent} = jorel_rpm_init_script_dtl:render(RPMData),
  InitScript = filename:join([RPMBuildDir, "SOURCES", RelName]),
  ?INFO("* Create file ~s", [InitScript]),
  ok = file:write_file(InitScript, iolist_to_binary(InitContent)),
  ?INFO("== Provider ~p complete", [?PROVIDER]),
  State.

