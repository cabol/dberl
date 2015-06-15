%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Carlos Andres Bolaños, Inc. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% @author Carlos Andres Bolaños R.A. <candres@niagara.io>
%%% @copyright (C) 2015, <Carlos Andres Bolaños>, All Rights Reserved.
%%% @doc
%%% Couchbase Suite.
%%% @end
%%%-------------------------------------------------------------------
-module(dberl_couchbase_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1]).

%% Tests
-export([basic/1, json_enc/1, query/1]).

-define(STORES, [dberl_test]).

%%%===================================================================
%%% Common Test
%%%===================================================================

all() ->
  [{group, basic_test_group}].

groups() ->
  [{basic_test_group, [sequence], [basic, json_enc, query]}].

init_per_suite(Config) ->
  application:ensure_all_started(dberl),
  Config.

end_per_suite(Config) ->
  [dberl_repo:flush(Store) || Store <- ?STORES],
  application:stop(dberl),
  Config.

%%%===================================================================
%%% Exported Test functions
%%%===================================================================

basic(_Config) ->
  run_all_providers(fun t_basic/1).

json_enc(_Config) ->
  run_all_providers(fun t_json_enc/1).

query(_Config) ->
  run_all_providers(fun t_query/1).

%%%===================================================================
%%% Test cases
%%%===================================================================

t_basic(Store) ->
  %% Debug
  ct:print("\e[1;96m[~p] 't_basic' testcase.\e[0m", [Store]),

  %% K/V
  Keys = [<<"K1">>, <<"K2">>, <<"K3">>],
  Values = [<<"V1">>, <<"V2">>, <<"V3">>],
  KV = lists:zip(Keys, Values),

  %% Store K/V
  ok = dberl_repo:set(Store, <<"K1">>, <<"V1">>),
  ok = dberl_repo:set(Store, <<"K2">>, <<"V2">>),

  %% Fetch stored keys
  <<"V1">> = dberl_repo:fetch(Store, <<"K1">>),
  <<"V2">> = dberl_repo:fetch(Store, <<"K2">>),

  %% Delete K2
  ok = dberl_repo:delete(Store, <<"K2">>),
  {error, notfound} = dberl_repo:fetch(Store, <<"K2">>),

  %% Delete K1
  ok = dberl_repo:delete(Store, <<"K1">>),
  {error, notfound} = dberl_repo:fetch(Store, <<"K1">>),

  %% Store bulk K/V
  R0 = lists:zip(Keys, [ok, ok, ok]),
  R0 = dberl_repo:set_bulk(Store, KV),

  %% Fetch bulk
  KV = dberl_repo:fetch_bulk(Store, Keys),

  %% Delete bulk
  R0 = dberl_repo:delete_bulk(Store, Keys),

  %% Fetch bulk
  R1 = lists:zip(Keys, [{error, notfound} || _X <- lists:seq(1, 3)]),
  R1 = dberl_repo:fetch_bulk(Store, Keys),

  ok.

t_json_enc(Store) ->
  %% Debug
  ct:print("\e[1;96m[~p] 't_json_enc' testcase.\e[0m", [Store]),

  %% Map
  M0 = #{a => a, b => <<"b">>, c => #{c1 => c1, c2 => 2}},

  %% Store Map in JSON format
  ok = dberl_repo:set(Store, <<"M0">>, M0),

  %% Fetch Map
  #{<<"a">> := <<"a">>,
    <<"b">> := <<"b">>,
    <<"c">> := #{<<"c1">> := <<"c1">>, <<"c2">> := 2}} =
    dberl_repo:fetch(Store, <<"M0">>),

  %% Delete stored map
  ok = dberl_repo:delete(Store, <<"M0">>),
  {error, notfound} = dberl_repo:fetch(Store, <<"M0">>),

  ok.

t_query(Store) ->
  %% Debug
  ct:print("\e[1;96m[~p] 't_query' testcase.\e[0m", [Store]),

  %% Docs
  {{Year, Month, _Day}, {_, _, _}} = calendar:universal_time(),
  TS1 = {{Year + 1, Month, 5}, {14, 0, 0}},
  TS2 = {{Year + 1, Month, 4}, {14, 0, 0}},
  TS3 = {{Year + 1, Month, 3}, {14, 0, 0}},
  TS4 = {{Year + 1, Month, 2}, {14, 0, 0}},
  TS5 = {{Year + 1, Month, 1}, {14, 0, 0}},
  D1 = dberl_util:to_bin(dberl_util:format_datetime(rfc1123, TS1)),
  D2 = dberl_util:to_bin(dberl_util:format_datetime(rfc1123, TS2)),
  D3 = dberl_util:to_bin(dberl_util:format_datetime(rfc1123, TS3)),
  D4 = dberl_util:to_bin(dberl_util:format_datetime(rfc1123, TS4)),
  D5 = dberl_util:to_bin(dberl_util:format_datetime(rfc1123, TS5)),
  M1 = #{id => <<"M1">>, a => a, b => <<"b">>, c => c, start => D1},
  M2 = #{id => <<"M2">>, a => a, b => <<"b1">>, c => c, start => D2},
  M3 = #{id => <<"M3">>, a => a1, b => <<"b1">>, c => c, start => D3},
  M4 = #{id => <<"M4">>, a => a1, b => <<"b1">>, c => c, start => D4},
  M5 = #{id => <<"M5">>, a => a2, b => <<"b2">>, c => c, start => D5},
  M6 = #{id => <<"M6">>, a => a1, b => <<"b1">>, c => c, start => D3},
  M7 = #{id => <<"M7">>, a => a1, b => <<"b1">>, c => c, start => D3},

  %% KV
  Keys = [<<"M1">>, <<"M2">>, <<"M3">>, <<"M4">>, <<"M5">>, <<"M6">>, <<"M7">>],
  Values = [M1, M2, M3, M4, M5, M6, M7],
  KV = lists:zip(Keys, Values),

  %% Set bulk
  R0 = lists:zip(Keys, [ok, ok, ok, ok, ok, ok, ok]),
  R0 = dberl_repo:set_bulk(Store, KV),

  %% Procedures
  create_procedures(Store),

  %% Query
  {7, All} = dberl_repo:find_all(Store, {"tests", "v0"}),
  {7, All} = dberl_repo:find_all(Store, {"tests", "v0"}, 100, 0),
  7 = length(All),
  {7, MQ} = dberl_repo:find_by_cond(Store, {"tests", "v1"}, [{a, "a"}]),
  2 = length(MQ),
  Doc = {"tests", "v2"},
  Conds = [{a, "a1"}, {b, "b1"}],
  {7, MQ0} = dberl_repo:find_by_cond(Store, Doc, Conds),
  4 = length(MQ0),
  [#{<<"id">> := <<"M3">>},
   #{<<"id">> := <<"M4">>},
   #{<<"id">> := <<"M6">>},
   #{<<"id">> := <<"M7">>}] = MQ0,

  %% Query with pagination
  {7, MQ1} = dberl_repo:find_by_cond(Store, Doc, Conds, 2, undefined),
  2 = length(MQ1),
  [#{<<"id">> := <<"M3">>}, #{<<"id">> := <<"M4">>}] = MQ1,
  {7, MQ2} = dberl_repo:find_by_cond(Store, Doc, Conds, 1, "M4"),
  1 = length(MQ2),
  [#{<<"id">> := <<"M6">>}] = MQ2,
  {7, MQ3} = dberl_repo:find_by_cond(Store, Doc, Conds, 1, "M6"),
  1 = length(MQ3),
  [#{<<"id">> := <<"M7">>}] = MQ3,

  %% Query by range
  Doc2 = {"tests", "v3"},
  RStart = [{c, "c"}, {startkey, [Year + 1, Month, 1, 14, 0, 0]}],
  REnd = [{c, "c"}, {endkey, [Year + 1, Month, 4, 23, 0, 0]}],
  {7, MQ4} = dberl_repo:find_by_range(Store, Doc2, RStart, REnd, 2, 0),
  [#{<<"id">> := <<"M5">>}, #{<<"id">> := <<"M4">>}] = MQ4,
  {7, MQ5} = dberl_repo:find_by_range(Store, Doc2, RStart, REnd, 2, 2),
  [#{<<"id">> := <<"M3">>}, #{<<"id">> := <<"M6">>}] = MQ5,
  {7, MQ6} = dberl_repo:find_by_range(Store, Doc2, RStart, REnd, 10, 4),
  [#{<<"id">> := <<"M7">>}, #{<<"id">> := <<"M2">>}] = MQ6,

  ok.

%%%===================================================================
%%% Internals
%%%===================================================================

run_all_providers(Fun) ->
  lists:foreach(Fun, ?STORES).

create_procedures(Store) ->
  Map0 = <<"function (doc, meta) {emit(meta.id, doc);}">>,
  Map1 = <<"function (doc, meta) {emit(doc.a, doc);}">>,
  Map2 = <<"function (doc, meta) {emit([doc.a, doc.b], doc);}">>,
  MapStr3 =
    "function (doc, meta) {" ++
      "if(doc.c && doc.start) {" ++
        "emit([doc.c, dateToArray(doc.start)], doc);" ++
    "}}",
  Map3 = list_to_binary(MapStr3),
  DDoc = #{v0 => #{map => Map0},
           v1 => #{map => Map1},
           v2 => #{map => Map2},
           v3 => #{map => Map3}},
  ok = dberl_repo:set_query(Store, "tests", DDoc),
  timer:sleep(1000).
