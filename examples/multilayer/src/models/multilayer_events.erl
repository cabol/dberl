-module(multilayer_events).

%% Event Specification.
-opaque event() ::
  #{
    id         => binary(),
    event_type => binary(),
    comment    => binary(),
    created_at => binary(),
    updated_at => binary(),
    type       => atom()
  }.

%% Exported types
-export_type([event/0]).

%% API
-export([new/3, from_map/1]).
-export([id/1, id/2,
         event_type/1, event_type/2,
         comment/1, comment/2,
         created_at/1, created_at/2,
         updated_at/1, updated_at/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(iolist(), iolist(), iolist()) -> event().
new(Id, EventType, Comment) ->
  Now = dberl_util:now(),
  #{
    id         => dberl_util:to_bin(Id),
    event_type => dberl_util:to_bin(EventType),
    comment    => dberl_util:to_bin(Comment),
    created_at => Now,
    updated_at => Now,
    type       => event
  }.

-spec from_map(map()) -> event().
from_map(Map) ->
  #{
    id         => maps:get(<<"id">>, Map, null),
    event_type => maps:get(<<"event_type">>, Map, null),
    comment    => maps:get(<<"comment">>, Map, null),
    created_at => maps:get(<<"created_at">>, Map, null),
    updated_at => maps:get(<<"updated_at">>, Map, null),
    type       => event
  }.

-spec id(event()) -> binary().
id(#{id := Val}) ->
  Val.

-spec id(event(), binary()) -> event().
id(Doc, Val) ->
  maps:put(id, Val, Doc).

-spec event_type(event()) -> binary().
event_type(#{event_type := Val}) ->
  Val.

-spec event_type(event(), binary()) -> event().
event_type(Doc, Val) ->
  maps:put(event_type, Val, Doc).

-spec comment(event()) -> binary().
comment(#{comment := Val}) ->
  Val.

-spec comment(event(), binary()) -> event().
comment(Doc, Val) ->
  maps:put(comment, Val, Doc).

-spec created_at(event()) -> binary().
created_at(#{created_at := Val}) ->
  Val.

-spec created_at(event(), binary()) -> event().
created_at(Doc, Val) ->
  maps:put(created_at, Val, Doc).

-spec updated_at(event()) -> binary().
updated_at(#{updated_at := Val}) ->
  Val.

-spec updated_at(event(), binary()) -> event().
updated_at(Doc, Val) ->
  maps:put(updated_at, Val, Doc).
