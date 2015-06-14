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
%%% JSON Utilities.
%%% @end
%%%-------------------------------------------------------------------
-module(dberl_json).

%% API
-export([encode/1, decode/1]).
-export([ejson_to_proplist/1, proplist_to_ejson/1, ejson_to_map/1]).

%% Types
-type json()      :: #{} | [#{}] | binary() | number() | boolean() | null.
-type proplist()  :: [{atom() | iolist() | string(), any()}].
-type json_term() :: [json_term()] | {[json_term()]} |
                     [{binary() | atom() | integer(), json_term()}] |
                     integer() | float() | binary() | atom().

-export_type([json/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec encode(json()) -> iodata().
encode(Json) -> jiffy:encode(Json, [uescape]).

-spec decode(iodata()) -> json().
decode(Data) ->
  try jiffy:decode(Data, [return_maps])
  catch
    _:{error, _} ->
      lager:warning("Bad Json: ~p", [Data]),
      throw(bad_json)
  end.

-spec ejson_to_proplist(json_term()) -> proplist().
ejson_to_proplist({TupleList}) ->
  ejson_to_proplist(TupleList, []);
ejson_to_proplist(TupleList) ->
  ejson_to_proplist(TupleList, []).

-spec proplist_to_ejson(proplist()) -> json_term().
proplist_to_ejson(TupleList) ->
  proplist_to_ejson(TupleList, []).

-spec ejson_to_map(json_term()) -> map().
ejson_to_map({TupleList}) ->
  ejson_to_map(TupleList, #{});
ejson_to_map(TupleList) ->
  ejson_to_map(TupleList, #{}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
ejson_to_proplist([], Acc) ->
  Acc;
ejson_to_proplist([{K, []} | T], Acc) ->
  ejson_to_proplist(T, [{K, [], []} | Acc]);
ejson_to_proplist([{K, {V = [{_, _} | _T0]}} | T], Acc) ->
  ejson_to_proplist(T, [{K, ejson_to_proplist(V, [])} | Acc]);
ejson_to_proplist([{K, V} | T], Acc) ->
  ejson_to_proplist(T, [{K, V} | Acc]).

%% @private
proplist_to_ejson([], Acc) ->
  {Acc};
proplist_to_ejson([{K, []} | T], Acc) ->
  proplist_to_ejson(T, [{K, <<>>} | Acc]);
proplist_to_ejson([{K, V = [{_, _} | _T0]} | T], Acc) ->
  proplist_to_ejson(T, [{K, proplist_to_ejson(V, [])} | Acc]);
proplist_to_ejson([{K, V} | T], Acc) ->
  proplist_to_ejson(T, [{K, V} | Acc]).

%% @private
ejson_to_map([], Acc) ->
  Acc;
ejson_to_map([{K, []} | T], Acc) ->
  ejson_to_map(T, maps:put(K, [], Acc));
ejson_to_map([{K, {V = [{_, _} | _T0]}} | T], Acc) ->
  ejson_to_map(T, maps:put(K, ejson_to_map(V, #{}), Acc));
ejson_to_map([{K, V} | T], Acc) ->
  ejson_to_map(T, maps:put(K, V, Acc)).
