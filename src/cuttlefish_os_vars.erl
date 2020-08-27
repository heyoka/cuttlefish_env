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

-export([check/0, map/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

map(Config) ->

    Prefix =
    case catch init:script_id() of
        {"Erlang/OTP", _Vsn} ->
            "";
        {ReleaseName, _Vsn} = Res ->
            lager:notice("release (init_script): ~p",[Res]),
            ReleaseName;
        What ->
            lager:error("permanent releases: ~p",[What]),
            ""
    end,

    lager:warning("script_id is : ~p",[Prefix]),

%%    Prefix =
%%    case catch release_handler:which_releases() of
%%        [{ReleaseName, _Version, _Applications, _Type}] = Res ->
%%            lager:notice("permanent releases: ~p",[Res]),
%%            ReleaseName;
%%        _ ->
%%            lager:error("permanent releases: ~p",[What]),
%%            ""
%%    end,
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


-endif.
