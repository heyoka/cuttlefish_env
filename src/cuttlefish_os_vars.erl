%% -------------------------------------------------------------------
%%
%% cuttlefish_advanced: handles merging of advanced configs
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(cuttlefish_os_vars).

-export([overlay/1, check/0, map/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

map(Config) ->
    Prefix =
    case catch release_handler:which_releases(permanent) of
        [{ReleaseName, _Version, _Applications, _Type}] -> ReleaseName;
        _ -> ""
    end,
    Updated = lists:foldl(
        fun({ConfigElementName, _ConfigElement}, Acc) ->
            %% check for os var and replace when set
            EnvKey = env_key(Prefix, ConfigElementName),
            lager:notice("~nEnvKey: ~p~n",[EnvKey]),
            case os:getenv(EnvKey) of
                false ->
                    Acc;
                EnvValue ->
%%                    io:format(" -- we have a value: ~p~n",[EnvValue]),
                    cuttlefish_util:replace_proplist_value(ConfigElementName, EnvValue, Acc)
            end
        end,
        Config,
        Config
    ),
    Updated.

%% replace with OS vars
overlay(GeneratedConfig) ->
    lists:foldl(
        fun({ApplicationName, GeneratedApplicationConfig}, OuterAcc) ->
%%            io:format("~nApp: ~p, Config: ~p~n", [ApplicationName, GeneratedApplicationConfig]),
%%            GeneratedApplicationConfig = proplists:get_value(ApplicationName, GeneratedConfig, []),
            Updated = lists:foldl(
                fun({ConfigElementName, _ConfigElement}, Acc) ->
                    %% check for os var and replace when set
                    EnvKey = env_key(ApplicationName, ConfigElementName),
%%                    io:format("~nEnvKey: ~p~n",[EnvKey]),
                    case os:getenv(EnvKey) of
                        false ->
                            Acc;
                        EnvValue ->
%%                            io:format(" -- we have a value: ~p~n",[EnvValue]),
                            cuttlefish_util:replace_proplist_value(ConfigElementName, EnvValue, Acc)
                    end
                end,
                GeneratedApplicationConfig,
                GeneratedApplicationConfig
            ),
            cuttlefish_util:replace_proplist_value(ApplicationName, Updated, OuterAcc)
        end,
        GeneratedConfig, GeneratedConfig).

env_key(ReleaseName, [_First|_]=ConfigElementName) when is_list(ReleaseName) andalso is_list(_First) ->
    string:to_upper(
        ReleaseName ++ "_" ++
        lists:flatten(lists:join("_", ConfigElementName))
    );
env_key(ApplicationName, ConfigElementName) when is_atom(ApplicationName) ->
    string:to_upper(
        atom_to_list(ApplicationName) ++ "_" ++
            lists:flatten(string:replace(atom_to_list(ConfigElementName), ".", "_", all))).


check() ->
    env_key("faxe", ["holy", "moly"]).

-ifdef(TEST).

map_os_vars_test() ->
    GeneratedConfig = [
        {["app1", "setting1", "1"], "value1.1"},
        {["app2", "setting2", "1"], "value2.2"}

    ],
    os:set_env_var("_APP2_SETTING2_1", "set_by_env_var"),
    os:set_env_var("_APP2_SETTING2_2", "set_by_env_var2"),

    Expected = [
        {["app1", "setting1", "1"], "value1.1"},
        {["app2", "setting2", "1"], "set_by_env_var"}
    ],
    NewConfig = map(GeneratedConfig),

    ?assertEqual(Expected, NewConfig),

    ok.

overlay_os_vars_test() ->
    GeneratedConfig = [
        {app1, [{'setting1.1', "value1.1"}]},
        {app2, [{'setting2.1', "value2.1"}]},
        {app3, [{'setting3.1', [{"blah", "blah"}, {"blarg", "blarg"}]}]}
    ],

    os:set_env_var("APP2_SETTING2_1", "set_by_env_var"),
    os:set_env_var("APP2_SETTING2_2", "set_by_env_var2"),

    Expected = [
        {app1, [{'setting1.1', "value1.1"}]},
        {app2, [{'setting2.1', "set_by_env_var"}]},
        {app3, [{'setting3.1', [{"blah", "blah"}, {"blarg", "blarg"}]}]}
    ],
    NewConfig = overlay(GeneratedConfig),

    ?assertEqual(Expected, NewConfig),

    ok.

-endif.
