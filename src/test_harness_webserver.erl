-module(test_harness_webserver).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, Port} = application:get_env(test_harness, webserver_port),
  MaxKeepAlive = application:get_env(test_harness, webserver_max_keepalive, 50),
  Timeout = application:get_env(test_harness, webserver_timeout, 5000),
  Dispatch = cowboy_router:compile(dispatch_rules()),
  {ok, _} = cowboy:start_clear(test_harness_cowboy,
                               [{port, Port}],
                               #{env => #{dispatch => Dispatch},
                                 max_keepalive => MaxKeepAlive,
                                 compress => true,
                                 timeout => Timeout}),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok = cowboy:stop_listener(test_harness_cowboy).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

dispatch_rules() ->
  [{'_', [% Speeds
          {"/speeds", test_harness_speeds_handler, []}]}].
