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
%%% Couchbase Provider.
%%% @end
%%%-------------------------------------------------------------------
-module(dberl_repo_couchbase).

-behaviour(dberl_repo).

%% Repository API
-export([init/1]).
-export([fetch/3, fetch_bulk/3]).
-export([set/4, set_bulk/3]).
-export([delete/3, delete_bulk/3]).
-export([flush/2]).

%% Query API
-export([find_all/3, find_all/5]).
-export([find_by_cond/4, find_by_cond/6]).
-export([find_by_range/5, find_by_range/7]).
-export([query/4, set_query/4]).

%% State
-record(state, {pool :: atom(), q_args :: [{atom(), any()}]}).
-type state() :: #state{}.

-type q_results()  :: {dberl_repo:total(), dberl_repo:results()}.
-type procedure()  :: #{map => binary(), reduce => binary()}.
-type procedures() :: #{procedure => procedure()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec init([{atom(), any()}]) -> {ok, state()}.
init(Options) ->
  PoolName = dberl_util:keyfind(poolname, Options, cb_default),
  PoolSize = dberl_util:keyfind(poolsize, Options, 5),
  Host = dberl_util:keyfind(host, Options, "localhost:8091"),
  User = dberl_util:keyfind(username, Options, ""),
  Password = dberl_util:keyfind(password, Options, ""),
  Bucket = dberl_util:keyfind(bucket, Options, "default"),
  Transcoder = dberl_util:keyfind(transcoder, Options, cberl_transcoder),
  CberArgs = dberl_util:keyfind(q_args, Options, []),
  {ok, _} = cberl:start_link(
    PoolName, PoolSize, Host, User, Password, Bucket, Transcoder),
  {ok, #state{pool = PoolName, q_args = CberArgs}}.

-spec fetch(
  dberl_repo:store(), dberl_repo:key(), state()
) -> dberl_repo:response(dberl_repo:value(), state()).
fetch(_Store, Key, #state{pool = PoolName} = State) ->
  case cberl_get(PoolName, Key) of
    {_, {error, E}} -> {error, E, State};
    {Key, Val}      -> {ok, Val, State}
  end.

-spec fetch_bulk(
  dberl_repo:store(), [dberl_repo:key()], state()
) -> dberl_repo:response(dberl_repo:bulk(), state()).
fetch_bulk(_Store, Keys, #state{pool = PoolName} = State) ->
  L = [cberl_get(PoolName, Key) || Key <- Keys],
  {ok, L, State}.

-spec set(
  dberl_repo:store(), dberl_repo:key(), dberl_repo:value(), state()
) -> dberl_repo:response(ok, state()).
set(_Store, Key, Value, #state{pool = PoolName} = State) ->
  case cberl_set(PoolName, Key, Value) of
    {_, {error, E}} -> {error, E, State};
    {Key, ok}       -> {ok, ok, State}
  end.

-spec set_bulk(
  dberl_repo:store(), [{dberl_repo:key(), dberl_repo:value()}], state()
) -> dberl_repo:response(dberl_repo:bulk(), state()).
set_bulk(_Store, KeyValuePairs, #state{pool = PoolName} = State) ->
  L = [cberl_set(PoolName, Key, Value) || {Key, Value} <- KeyValuePairs],
  {ok, L, State}.

-spec delete(
  dberl_repo:store(), dberl_repo:key(), state()
) -> dberl_repo:response(ok, state()).
delete(_Store, Key, #state{pool = PoolName} = State) ->
  case cberl_del(PoolName, Key) of
    {Key, {error, E}} -> {error, E, State};
    {Key, ok}         -> {ok, ok, State}
  end.

-spec delete_bulk(
  dberl_repo:store(), [dberl_repo:key()], state()
) -> dberl_repo:response(dberl_repo:bulk(), state()).
delete_bulk(_Store, Keys, #state{pool = PoolName} = State) ->
  L = [cberl_del(PoolName, Key) || Key <- Keys],
  {ok, L, State}.

%%%===================================================================
%%% Store API
%%%===================================================================

-spec flush(dberl_repo:store(), state()) -> dberl_repo:response(ok, state()).
flush(Store, #state{pool = PoolName} = State) ->
  Ret = cberl:flush(PoolName, dberl_util:to_list(Store)),
  {ok, Ret, State}.

%%%===================================================================
%%% Query API
%%%===================================================================

-spec find_all(
  dberl_repo:store(), {string(), string()}, state()
) -> dberl_repo:response(q_results(), state()).
find_all(Store, Doc, State) ->
  find_all(Store, Doc, 0, 0, State).

-spec find_all(
  dberl_repo:store(),
  {string(), string()},
  non_neg_integer(),
  dberl_repo:offset(),
  state()
) -> dberl_repo:response(q_results(), state()).
find_all(Store, Doc, Limit, Offset, #state{q_args = QArgs} = State) ->
  Args = parse_args([{offset, Offset}, {limit, Limit}], QArgs),
  query(Store, Doc, Args, State).

-spec find_by_cond(
  dberl_repo:store(), {string(), string()}, dberl_repo:proplist(), state()
) -> dberl_repo:response(q_results(), state()).
find_by_cond(Store, Doc, Conditions, State) ->
  find_by_cond(Store, Doc, Conditions, 0, 0, State).

-spec find_by_cond(
  dberl_repo:store(),
  {string(), string()},
  dberl_repo:proplist(),
  non_neg_integer(),
  dberl_repo:offset(),
  state()
) -> dberl_repo:response(q_results(), state()).
find_by_cond(Store,
             Doc,
             Conditions,
             Limit,
             Offset,
             #state{q_args = QArgs} = State) ->
  Keys = case Conditions of
           [_Cond] -> {key, Conditions};
           _       -> {keys, Conditions}
         end,
  Args = parse_args([Keys, {offset, Offset}, {limit, Limit}], QArgs),
  query(Store, Doc, Args, State).

-spec find_by_range(
  dberl_repo:store(),
  {string(), string()},
  dberl_repo:proplist(),
  dberl_repo:proplist(),
  state()
) -> dberl_repo:response(q_results(), state()).
find_by_range(Store, Doc, Start, End, State) ->
  find_by_range(Store, Doc, Start, End, 0, 0, State).

-spec find_by_range(
  dberl_repo:store(),
  {string(), string()},
  dberl_repo:proplist(),
  dberl_repo:proplist(),
  non_neg_integer(),
  dberl_repo:offset(),
  state()
) -> dberl_repo:response(q_results(), state()).
find_by_range(Store,
              Doc,
              Start,
              End,
              Limit,
              Offset,
              #state{q_args = QArgs} = State) ->
  Args = parse_args(
    [{startkey, Start},
     {endkey, End},
     {offset, Offset},
     {limit, Limit}],
    QArgs),
  query(Store, Doc, Args, State).

-spec query(
  dberl_repo:store(), {string(), string()}, dberl_repo:proplist(), state()
) -> dberl_repo:response(q_results(), state()).
query(_Store, {DocName, ViewName}, Args, #state{pool = PoolName} = State) ->
  case cberl:view(PoolName, DocName, ViewName, Args) of
    {ok, {TotalRows, Rows}} ->
      {ok, {TotalRows, dec_q_results(Rows)}, State};
    {ok, {Rows}} ->
      R = dec_q_results(Rows),
      {ok, {length(R), R}, State};
    {error, E} ->
      {error, E, State}
  end.

-spec set_query(
  dberl_repo:store(), iolist(), procedures(), state()
) -> dberl_repo:response(ok, state()).
set_query(_Store, DocName, Procedures, #state{pool = PoolName} = State) ->
  Ret = cberl:set_design_doc(
    PoolName, dberl_util:to_list(DocName), #{views => Procedures}),
  {ok, Ret, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
cberl_get(PoolName, Key) ->
  case cberl:get(PoolName, Key) of
    {Key, _, Val}            -> {Key, Val};
    {_, {error, key_enoent}} -> {Key, {error, notfound}};
    {_, {error, _} = E}      -> throw(E);
    {error, _} = E           -> throw(E)
  end.

%% @private
cberl_set(PoolName, Key, Value) ->
  case cberl:set(PoolName, Key, 0, Value) of
    ok             -> {Key, ok};
    {error, _} = E -> throw(E)
  end.

%% @private
cberl_del(PoolName, Key) ->
  case cberl:remove(PoolName, Key) of
    ok                  -> {Key, ok};
    {error, key_enoent} -> {Key, ok};
    {error, _} = E      -> throw(E)
  end.

%% @private
dec_q_results(L) ->
  F = fun(E, Acc) ->
        #{<<"value">> := Val} = dberl_json:ejson_to_map(E),
        [Val | Acc]
      end,
  lists:foldr(F, [], L).

%% @private
normalize_conds(Opts) ->
  F = fun({_, Y}, Acc) when is_list(Y) ->
        case io_lib:printable_list(Y) of
          true  -> [dberl_util:to_bin(Y) | Acc];
          false -> [Y | Acc]
        end;
      ({_, Y}, Acc) ->
        [Y | Acc]
      end,
  lists:foldr(F, [], Opts).

%% @private
parse_args([], Acc) ->
  Acc;
parse_args([{key, V} | T], Acc) ->
  [Key] = normalize_conds(V),
  parse_args(T, [{key, Key} | Acc]);
parse_args([{keys, V} | T], Acc) ->
  Keys = normalize_conds(V),
  parse_args(T, [{keys, [Keys]} | Acc]);
parse_args([{startkey, V} | T], Acc) ->
  StartKeys = dberl_util:to_list(dberl_json:encode(normalize_conds(V))),
  parse_args(T, [{startkey, StartKeys} | Acc]);
parse_args([{endkey, V} | T], Acc) ->
  EndKeys = dberl_util:to_list(dberl_json:encode(normalize_conds(V))),
  parse_args(T, [{endkey, EndKeys} | Acc]);
parse_args([{offset, V} | T], Acc) when is_integer(V), V >= 0 ->
  parse_args(T, [{skip, V} | Acc]);
parse_args([{offset, V} | T], Acc) when is_list(V), length(V) =< 0 ->
  parse_args(T, [{skip, 0} | Acc]);
parse_args([{offset, V} | T], Acc) when is_list(V), length(V) > 0 ->
  parse_args(T, [{startkey_docid, V}, {skip, 1}] ++ Acc);
parse_args([{limit, V} | T], Acc) when is_integer(V), V > 0 ->
  parse_args(T, [{limit, V} | Acc]);
parse_args([_ | T], Acc) ->
  parse_args(T, Acc).
