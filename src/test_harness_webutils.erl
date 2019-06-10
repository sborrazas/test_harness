-module(test_harness_webutils).

%% API
-export([invalid_json/1,
         invalid_content/2,
         not_found/1,
         reply/3,
         json_body/1,
         method/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec invalid_json(cowboy_req:req()) -> cowboy_req:req().
invalid_json(Req) ->
  reply(Req, 400, #{<<"message">> => <<"Invalid JSON body.">>}).

-spec not_found(cowboy_req:req()) -> cowboy_req:req().
not_found(Req) ->
  reply(Req, 404, #{<<"message">> => <<"Not found.">>}).

-spec invalid_content(cowboy_req:req(), binary()) -> cowboy_req:req().
invalid_content(Req, Reason) ->
  reply(Req, 422, #{<<"error">> => Reason}).

-spec reply(cowboy_req:req(), non_neg_integer(), term()) -> cowboy_req:req().
reply(Req, StatusCode, Content) ->
  cowboy_req:reply(StatusCode, #{}, jiffy:encode(Content), Req).

%% Helpers
-spec json_body(cowboy_req:req()) -> {ok, any()} | error.
json_body(Req) ->
  {ok, Body, _Req} = cowboy_req:read_body(Req),
  try
    jiffy:decode(Body, [return_maps])
  of
    Content -> {ok, Content}
  catch
    error:_ -> error
  end.

-spec method(cowboy_req:req()) -> binary().
method(Req) ->
  case cowboy_req:header(<<"access-control-request-method">>, Req) of
    undefined -> cowboy_req:method(Req);
    Method -> Method
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
