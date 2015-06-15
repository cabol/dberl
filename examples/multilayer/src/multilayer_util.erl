-module(multilayer_util).

%% API
-export([enc_json/1, dec_json/1]).
-export([handle_exception/3]).

%% Types
-type json() :: #{} | [#{}] | binary() | number() | boolean() | null.

%%%===================================================================
%%% API
%%%===================================================================

-spec enc_json(json()) -> iodata().
enc_json(Json) -> jiffy:encode(Json, [uescape]).

-spec dec_json(iodata()) -> json().
dec_json(Data) ->
  try jiffy:decode(Data, [return_maps])
  catch
    _:{error, _} ->
      lager:warning("Bad Json: ~p", [Data]),
      throw(bad_json)
  end.

-spec handle_exception(
  atom(), cowboy_req:req(), term()
) -> {false | halt, cowboy_req:req(), term()}.
handle_exception(bad_request, Req, State) ->
  {false, Req, State};
handle_exception(bad_type, Req, State) ->
  {false, Req, State};
handle_exception(badarg, Req, State) ->
  {false, Req, State};
handle_exception(bad_key, Req, State) ->
  {false, Req, State};
handle_exception(bad_json, Req, State) ->
  Req1 = set_resp_error(bad_json, Req),
  {false, Req1, State};
handle_exception({missing_query_param, Param}, Req, State) ->
  Req1 = set_resp_error(#{missing_query_param => Param}, Req),
  {false, Req1, State};
handle_exception({missing_field, Field}, Req, State) ->
  Req1 = set_resp_error(#{missing_field => Field}, Req),
  {false, Req1, State};
handle_exception(notfound, Req, State) ->
  Req1 = cowboy_req:reply(404, Req),
  {halt, Req1, State};
handle_exception(Reason, Req, State) ->
  lager:error("~p. Stack Trace: ~p", [Reason, erlang:get_stacktrace()]),
  Req1 =
    try cowboy_req:reply(500, Req)
    catch
      _:Error ->
        Msg = "~p Stack Trace: ~p~n",
        lager:critical(Msg, [Error, erlang:get_stacktrace()]),
        {ok, Req}
    end,
  {halt, Req1, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
set_resp_error(Error, Req) ->
  ResBody = multilayer_util:enc_json(#{error => Error}),
  cowboy_req:set_resp_body(ResBody, Req).
