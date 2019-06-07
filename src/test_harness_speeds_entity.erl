-module(test_harness_speeds_entity).

%% API
-export([log/1,
         list/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec log(map()) -> {ok, map()} | {error, binary()}.
log(#{<<"speed">> := Speed, <<"timestamp">> := Timestamp}) when is_number(Speed) andalso
                                                                is_binary(Timestamp) ->
  case parse_datetime(Timestamp) of
    {ok, TimestampDatetime} ->
      TimestampStr = iso8601:format(TimestampDatetime),
      SpeedEntity = test_harness_db:create(speed,
                                           #{speed => Speed, timestamp => TimestampStr}),
      {ok, SpeedEntity};
    invalid -> {error, <<"Invalid `datetime` format.">>}
  end;
log(_Params) ->
  {error, <<"`speed` (float) and `datetime` (string) parameters are required.">>}.

-spec list() -> [map()].
list() ->
  test_harness_db:list(speed).

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_datetime(Value) ->
  {ok, iso8601:parse(Value)}.
