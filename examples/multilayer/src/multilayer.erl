-module(multilayer).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, start_phase/3, stop/0, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec start() -> {ok, _} | {error, term()}.
start() ->
  application:ensure_all_started(multilayer).

-spec start(application:start_type(), any()) -> {ok, pid()} | {error, term()}.
start(_Type, _Args) ->
  multilayer_sup:start_link().

-spec start_phase(atom(), application:start_type(), []) -> ok | {error, _}.
start_phase(start_cowboy_listeners, _StartType, []) ->
  Routes     = cowboy_routes(),
  Dispatch   = cowboy_router:compile(Routes),
  ProtoOpts  = [{env, [{dispatch, Dispatch}]}],
  TransOpts  = application:get_env(multilayer, http_trans_opts, [{port, 8888}]),
  Cacceptors = application:get_env(multilayer, http_listener_count, 10),
  cowboy:start_http(http, Cacceptors, TransOpts, ProtoOpts),
  ok.

-spec stop() -> ok | {error, term()}.
stop() ->
  application:stop(multilayer).

-spec stop(atom()) -> ok.
stop(_State) ->
  ok.

%%%===================================================================
%%% Internals
%%%===================================================================

%% @private
cowboy_routes() ->
  [
   {'_',
    [
     {"/events/[:id]", multilayer_events_handler, []}
    ]
   }
  ].
