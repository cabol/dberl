-module(multilayer_events_repo).

%% API
-export([add/1, put/1, get/1, remove/1]).
-export([find_by_event_type/1]).

-define(STORE, multilayer_events).

%%%===================================================================
%%% API
%%%===================================================================

-spec add(multilayer_events:event()) -> ok.
add(Event) ->
  dberl_repo:set(?STORE, multilayer_events:id(Event), Event).

-spec put(multilayer_events:event()) -> ok.
put(Event) ->
  dberl_repo:set(?STORE, multilayer_events:id(Event), Event).

-spec get(iolist()) -> multilayer_events:event().
get(Id) ->
  case dberl_repo:fetch(?STORE, Id) of
    {error, notfound} ->
      notfound;
    Value when is_binary(Value) ->
      multilayer_events:from_map(dberl_json:decode(Value));
    Value ->
      multilayer_events:from_map(Value)
  end.

-spec remove(iolist()) -> ok.
remove(Id) ->
  dberl_repo:delete(?STORE, Id).

-spec find_by_event_type(binary()) -> [multilayer_events:event()].
find_by_event_type(EventType) ->
  Doc = {"queries", "find_by_event_type"},
  case dberl_repo:find_by_cond(?STORE, Doc, [{event_type, EventType}]) of
    {error, E} -> throw(E);
    {_, Reply} -> Reply
  end.
