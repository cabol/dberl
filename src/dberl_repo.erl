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
%%% Repository.
%%% @end
%%%-------------------------------------------------------------------
-module(dberl_repo).

-behaviour(gen_server).

%% Repository API
-export([start_link/3]).
-export([fetch/2, fetch_bulk/2]).
-export([set/3, set_bulk/2]).
-export([delete/2, delete_bulk/2]).
-export([flush/1]).

%% Query API
-export([find_all/2, find_all/4]).
-export([find_by_cond/3, find_by_cond/5]).
-export([find_by_range/4, find_by_range/6]).
-export([query/3, set_query/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% Types
%%%===================================================================

-type store()        :: atom().
-type key()          :: atom() | string() | binary().
-type value()        :: any().
-type error()        :: {error, term()}.
-type proplist()     :: [{atom(), any()}].
-type doc()          :: atom() | iolist() | tuple().
-type reply()        :: ok | value() | error().
-type offset()       :: non_neg_integer() | iolist().
-type bulk()         :: [{key(), reply()}].
-type results()      :: [{key(), value()}].
-type total()        :: non_neg_integer().
-type response(R, S) :: {ok, R, S} | {error, term(), S}.

-export_type([store/0, key/0, value/0, proplist/0, reply/0, offset/0,
              bulk/0, results/0, response/2]).

%% State
-record(state, {handler       = undefined :: module(),
                handler_state = undefined :: any()}).

%%%===================================================================
%%% Callback
%%%===================================================================

-callback init(Options) -> Response when
  Options  :: proplist(),
  Response :: {ok, term()}.

-callback fetch(Store, Key, State) -> Response when
  Store    :: store(),
  Key      :: key(),
  State    :: term(),
  Response :: response(value(), State).

-callback fetch_bulk(Store, Keys, State) -> Response when
  Store    :: store(),
  Keys     :: [key()],
  State    :: term(),
  Response :: response(bulk(), State).

-callback set(Store, Key, Value, State) -> Response when
  Store    :: store(),
  Key      :: key(),
  Value    :: value(),
  State    :: term(),
  Response :: response(ok, State).

-callback set_bulk(Store, KeyValuePairs, State) -> Response when
  Store         :: store(),
  KeyValuePairs :: [{key(), value()}],
  State         :: term(),
  Response      :: response(bulk(), State).

-callback delete(Store, Key, State) -> Response when
  Store    :: store(),
  Key      :: key(),
  State    :: term(),
  Response :: response(ok, State).

-callback delete_bulk(Store, Keys, State) -> Response when
  Store    :: store(),
  Keys     :: [key()],
  State    :: term(),
  Response :: response(bulk(), State).

-callback flush(Store, State) -> Response when
  Store    :: store(),
  State    :: term(),
  Response :: response(ok, State).

-callback find_all(Store, Doc, State) -> Response when
  Store    :: store(),
  Doc      :: doc(),
  State    :: term(),
  Response :: response({total(), results()}, State).

-callback find_all(Store, Doc, Limit, Offset, State) -> Response when
  Store    :: store(),
  Doc      :: doc(),
  Limit    :: non_neg_integer(),
  Offset   :: offset(),
  State    :: term(),
  Response :: response({total(), results()}, State).

-callback find_by_cond(Store, Doc, Conditions, State) -> Response when
  Store      :: store(),
  Doc        :: doc(),
  Conditions :: proplist(),
  State      :: term(),
  Response   :: response({total(), results()}, State).

-callback find_by_cond(
  Store, Doc, Conditions, Limit, Offset, State
) -> Response when
  Store      :: store(),
  Doc        :: doc(),
  Conditions :: proplist(),
  Limit      :: non_neg_integer(),
  Offset     :: offset(),
  State      :: term(),
  Response   :: response({total(), results()}, State).

-callback find_by_range(Store, Doc, Start, End, State) -> Response when
  Store    :: store(),
  Doc      :: doc(),
  Start    :: proplist(),
  End      :: proplist(),
  State    :: term(),
  Response :: response({total(), results()}, State).

-callback find_by_range(
  Store, Doc, Start, End, Limit, Offset, State
) -> Response when
  Store    :: store(),
  Doc      :: doc(),
  Start    :: proplist(),
  End      :: proplist(),
  Limit    :: non_neg_integer(),
  Offset   :: offset(),
  State    :: term(),
  Response :: response({total(), results()}, State).

-callback query(Store, Doc, Args, State) -> Response when
  Store    :: store(),
  Doc      :: doc(),
  Args     :: proplist(),
  State    :: term(),
  Response :: response({total(), results()}, State).

-callback set_query(Store, DocName, Procedures, State) -> Response when
  Store      :: store(),
  DocName    :: atom() | iolist(),
  Procedures :: map(),
  State      :: term(),
  Response   :: response(ok, State).

%%%===================================================================
%%% Repository API
%%%===================================================================

-spec start_link(atom(), module(), [term()]) -> {ok, pid()}.
start_link(Name, Module, Options) ->
  gen_server:start_link({local, Name}, ?MODULE, [Module, Options], []).

-spec fetch(store(), key()) -> reply().
fetch(Store, Key) ->
  gen_server:call(Store, {fetch, Store, Key}).

-spec fetch_bulk(store(), [key()]) -> bulk().
fetch_bulk(Store, Keys) ->
  gen_server:call(Store, {fetch_bulk, Store, Keys}).

-spec set(store(), key(), value()) -> reply().
set(Store, Key, Value) ->
  gen_server:call(Store, {set, Store, Key, Value}).

-spec set_bulk(store(), [{key(), value()}]) -> bulk().
set_bulk(Store, KeyValuePairs) ->
  gen_server:call(Store, {set_bulk, Store, KeyValuePairs}).

-spec delete(store(), key()) -> reply().
delete(Store, Key) ->
  gen_server:call(Store, {delete, Store, Key}).

-spec delete_bulk(store(), [key()]) -> bulk().
delete_bulk(Store, Keys) ->
  gen_server:call(Store, {delete_bulk, Store, Keys}).

-spec flush(store()) -> reply().
flush(Store) ->
  gen_server:call(Store, {flush, Store}).

%%%===================================================================
%%% Query API
%%%===================================================================

-spec find_all(store(), doc()) -> {total(), results()}.
find_all(Store, Doc) ->
  gen_server:call(Store, {find_all, Store, Doc}).

-spec find_all(
  store(), doc(), non_neg_integer(), offset()
) -> {total(), results()}.
find_all(Store, Doc, Limit, Offset) ->
  gen_server:call(Store, {find_all, Store, Doc, Limit, Offset}).

-spec find_by_cond(store(), doc(), proplist()) -> {total(), results()}.
find_by_cond(Store, Doc, Conditions) ->
  gen_server:call(Store, {find_by_cond, Store, Doc, Conditions}).

-spec find_by_cond(
  store(), doc(), proplist(), non_neg_integer(), offset()
) -> {total(), results()}.
find_by_cond(Store, Doc, Conditions, Limit, Offset) ->
  gen_server:call(Store, {find_by_cond, Store, Doc, Conditions, Limit, Offset}).

-spec find_by_range(
  store(), doc(), proplist(), proplist()
) -> {total(), results()}.
find_by_range(Store, Doc, Start, End) ->
  gen_server:call(Store, {find_by_range, Store, Doc, Start, End}).

-spec find_by_range(
  store(), doc(), proplist(), proplist(), non_neg_integer(), offset()
) -> {total(), results()}.
find_by_range(Store, Doc, Start, End, Limit, Offset) ->
  gen_server:call(
    Store, {find_by_range, Store, Doc, Start, End, Limit, Offset}).

-spec query(store(), doc(), proplist()) -> {total(), results()}.
query(Store, Doc, Args) ->
  gen_server:call(Store, {query, Store, Doc, Args}).

-spec set_query(store(), doc(), map()) -> reply().
set_query(Store, Doc, Procedures) ->
  gen_server:call(Store, {set_query, Store, Doc, Procedures}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
init([Module, Options]) ->
  {ok, HState} = Module:init(Options),
  {ok, #state{handler = Module, handler_state = HState}}.

%% @hidden
handle_call({fetch, Store, Key},
            _From,
            #state{handler = Handler, handler_state = HState} = State) ->
  handle_response(Handler:fetch(Store, Key, HState), State);
handle_call({fetch_bulk, Store, Keys},
            _From,
            #state{handler = Handler, handler_state = HState} = State) ->
  handle_response(Handler:fetch_bulk(Store, Keys, HState), State);
handle_call({set, Store, Key, Value},
            _From,
            #state{handler = Handler, handler_state = HState} = State) ->
  handle_response(Handler:set(Store, Key, Value, HState), State);
handle_call({set_bulk, Store, KeyValuePairs},
            _From,
            #state{handler = Handler, handler_state = HState} = State) ->
  handle_response(Handler:set_bulk(Store, KeyValuePairs, HState), State);
handle_call({delete, Store, Key},
            _From,
            #state{handler = Handler, handler_state = HState} = State) ->
  handle_response(Handler:delete(Store, Key, HState), State);
handle_call({delete_bulk, Store, Keys},
            _From,
            #state{handler = Handler, handler_state = HState} = State) ->
  handle_response(Handler:delete_bulk(Store, Keys, HState), State);
handle_call({flush, Store},
            _From,
            #state{handler = Handler, handler_state = HState} = State) ->
  handle_response(Handler:flush(Store, HState), State);
handle_call({find_all, Store, Doc},
            _From,
            #state{handler = Handler, handler_state = HState} = State) ->
  handle_response(Handler:find_all(Store, Doc, HState), State);
handle_call({find_all, Store, Doc, Limit, Offset},
            _From,
            #state{handler = Handler, handler_state = HState} = State) ->
  handle_response(Handler:find_all(Store, Doc, Limit, Offset, HState), State);
handle_call({find_by_cond, Store, Doc, Conditions},
            _From,
            #state{handler = Handler, handler_state = HState} = State) ->
  handle_response(Handler:find_by_cond(Store, Doc, Conditions, HState), State);
handle_call({find_by_cond, Store, Doc, Conditions, Limit, Offset},
            _From,
            #state{handler = Handler, handler_state = HState} = State) ->
  handle_response(
    Handler:find_by_cond(Store, Doc, Conditions, Limit, Offset, HState),
    State);
handle_call({find_by_range, Store, Doc, Start, End},
            _From,
            #state{handler = Handler, handler_state = HState} = State) ->
  handle_response(Handler:find_by_range(Store, Doc, Start, End, HState), State);
handle_call({find_by_range, Store, Doc, Start, End, Limit, Offset},
            _From,
            #state{handler = Handler, handler_state = HState} = State) ->
  handle_response(
    Handler:find_by_range(Store, Doc, Start, End, Limit, Offset, HState),
    State);
handle_call({query, Store, Procedure, Args},
            _From,
            #state{handler = Handler, handler_state = HState} = State) ->
  handle_response(Handler:query(Store, Procedure, Args, HState), State);
handle_call({set_query, Store, Doc, Procedures},
            _From,
            #state{handler = Handler, handler_state = HState} = State) ->
  handle_response(Handler:set_query(Store, Doc, Procedures, HState), State).

%% @hidden
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
  {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
  ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
handle_response(Response, State) ->
  case Response of
    {ok, Results, NewState} ->
      {reply, Results, State#state{handler_state = NewState}};
    {error, Error, NewState} ->
      {reply, {error, Error}, State#state{handler_state = NewState}}
  end.
