-module(test_harness_speeds_handler).

%% cowboy_http_handler callbacks
-export([init/2,
         terminate/3]).

-record(state, {}).

%%%===================================================================
%%% cowboy_http_handler callbacks
%%%===================================================================

init(Req, _Opts) ->
  Method = test_harness_webutils:method(Req),
  handle_req(Method, Req, #state{}).

terminate(_Reason, _Req, _State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_req(<<"POST">>, Req, State) ->
  case test_harness_webutils:json_body(Req) of
    {ok, Params} -> log_speed(Req, State, Params);
    error -> {ok, test_harness_webutils:invalid_json(Req), State}
  end;
handle_req(<<"GET">>, Req, State) ->
  Speeds = test_harness_speeds_entity:list(),
  {ok, test_harness_webutils:reply(Req, 200, Speeds), State};
handle_req(_Method, Req, State) ->
  {ok, test_harness_webutils:not_found(Req), State}.

log_speed(Req, State, Params) ->
  case test_harness_speeds_entity:log(Params) of
    {ok, Speed} -> {ok, test_harness_webutils:reply(Req, 200, Speed), State};
    {error, Reason} ->  {ok, test_harness_webutils:invalid_content(Req, Reason), State}
  end.
