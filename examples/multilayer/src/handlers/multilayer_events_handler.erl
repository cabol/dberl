-module(multilayer_events_handler).

%% Cowboy behavior
-export([init/2,
         allowed_methods/2,
         is_authorized/2,
         resource_exists/2,
         content_types_provided/2,
         content_types_accepted/2,
         delete_resource/2]).

%% Handlers
-export([handle_post/2, handle_get/2]).

%%%===================================================================
%%% Cowboy callbacks
%%%===================================================================

%% @hidden
init(Req, _Opts) ->
  {cowboy_rest, Req, #{}}.

%% @hidden
allowed_methods(Req, State) ->
  {[<<"POST">>, <<"GET">>, <<"DELETE">>], Req, State}.

%% @hidden
is_authorized(Req, State) ->
  {true, Req, State}.

%% @hidden
resource_exists(Req, State) ->
  Method = cowboy_req:method(Req),
  Id = cowboy_req:binding(id, Req),
  case {Method, Id} of
    {<<"POST">>, _} ->
      {false, Req, State};
    {<<"GET">>, undefined} ->
      {true, Req, State#{event => undefined}};
    {_, undefined} ->
      {false, Req, State};
    {_, _} ->
      case multilayer_events_repo:get(Id) of
        notfound ->
          {false, Req, State};
        Event ->
          {true, Req, State#{event => Event}}
      end
  end.

%% @hidden
content_types_accepted(Req, State) ->
  Function = case cowboy_req:method(Req) of
               <<"POST">> -> handle_post;
               <<"PUT">>  -> handle_put
             end,
  ContentTypes = [{{<<"application">>, <<"json">>, '*'}, Function}],
  {ContentTypes, Req, State}.

%% @hidden
content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_get}], Req, State}.

%% @hidden
delete_resource(Req, State) ->
  lager:debug("DELETE ~p~n", [cowboy_req:path(Req)]),
  Id = cowboy_req:binding(id, Req),
  multilayer_events_repo:remove(Id),
  {true, Req, State}.

%% @hidden
handle_post(Req, State) ->
  lager:debug("POST ~p~n> Query string: ~p~n",
              [cowboy_req:path(Req), cowboy_req:qs(Req)]),
  #{event_type := EventType, comment := Comment} = cowboy_req:match_qs(
    [{event_type, nonempty, null}, {comment, nonempty, null}], Req),
  case EventType of
    null ->
      multilayer_util:handle_exception(
        {missing_query_param, <<"event_type">>}, Req, State);
    _ ->
      Id = dberl_util:rand(16),
      Event = multilayer_events:new(Id, EventType, Comment),
      ok = multilayer_events_repo:put(Event),
      {{true, binary:list_to_bin(["/events/", Id])}, Req, State}
  end.

%% @hidden
handle_get(Req, #{event := undefined} = State) ->
  lager:debug("GET ~p~n> Query string: ~p~n",
              [cowboy_req:path(Req), cowboy_req:qs(Req)]),
  #{event_type := EventType} = cowboy_req:match_qs(
    [{event_type, nonempty, <<"">>}], Req),
  case EventType of
    null ->
      multilayer_util:handle_exception(
        {missing_query_param, <<"event_type">>}, Req, State);
    _ ->
      L = multilayer_events_repo:find_by_event_type(EventType),
      Json = multilayer_util:enc_json(L),
      {Json, Req, State}
  end;
handle_get(Req, #{event := Event} = State) ->
  lager:debug("GET ~p~n", [cowboy_req:path(Req)]),
  Json = multilayer_util:enc_json(Event),
  {Json, Req, State}.
