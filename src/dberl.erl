%%%-------------------------------------------------------------------
%%% @author Carlos Andres Bolaños R.A. <candres@niagara.io>
%%% @copyright (C) 2015, <Niagara Systems, Inc.>, All Rights Reserved.
%%% @doc
%%% Main app.
%%% @end
%%%-------------------------------------------------------------------
-module(dberl).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec start(term(), term()) -> {error, term()} | {ok, pid()}.
start(_Type, _Args) ->
  dberl_repo_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
  ok.
