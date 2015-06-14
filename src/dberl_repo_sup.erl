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
%%% Repo Supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(dberl_repo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Module, Options),
  {
   Name,
   {dberl_repo, start_link, [Name, Module, Options]},
   permanent,
   5000,
   worker,
   [Module]
  }
).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @hidden
init([]) ->
  {ok, Stores} = application:get_env(dberl, stores),
  Fun = fun({Name, Module, Options}) -> ?CHILD(Name, Module, Options) end,
  Children = lists:map(Fun, Stores),
  {ok, {{one_for_one, 5, 10}, Children}}.
