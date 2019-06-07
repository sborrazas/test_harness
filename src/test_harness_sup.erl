-module(test_harness_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  SupFlags = #{strategy => one_for_one,
               intensity => 1,
               period => 5},
  Webserver = #{id => webserver,
                start => {test_harness_webserver, start_link, []},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [test_harness_webserver]},
  Database = #{id => db,
               start => {test_harness_db, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [test_harness_db]},
  {ok, {SupFlags, [Webserver, Database]}}.

%%====================================================================
%% Internal functions
%%====================================================================