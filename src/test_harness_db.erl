-module(test_harness_db).

-behaviour(gen_server).

%% API
-export([start_link/0,
         create/2,
         list/1,
         delete_all/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {dir :: string(),
                interval :: timer:tref(),
                entities :: #{atom() => binary()}}).

%% Macros
-define(SERVER, ?MODULE).
-define(INTERNAL_TIMESPAN, 60000). % 1min
-define(MAX_ENTITIES_SIZE, 1024).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec create(atom(), map()) -> map().
create(EntityName, Attrs) ->
  gen_server:call(?SERVER, {create, EntityName, Attrs}).

-spec list(atom()) -> [map()].
list(EntityName) ->
  gen_server:call(?SERVER, {list, EntityName}).

-spec delete_all() -> ok.
delete_all() ->
  gen_server:call(?SERVER, delete_all).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, Dir} = application:get_env(test_harness, db_dir),
  {ok, Interval} = timer:send_interval(?INTERNAL_TIMESPAN, flush),
  {ok, #state{dir = Dir, interval = Interval, entities = #{}}}.

handle_call({create, EntityName, Attrs}, _From, State = #state{dir = Dir,
                                                               entities = Entities}) ->
  EncodedEntities = maps:get(EntityName, Entities, <<>>),
  EncodedEntities2 = <<(jiffy:encode(Attrs))/binary, "\n", EncodedEntities/binary>>,
  NewState =
    case byte_size(EncodedEntities2) >= ?MAX_ENTITIES_SIZE of
      true ->
        flush_entities(Dir, EntityName, EncodedEntities2),
        State#state{entities = maps:remove(EntityName, Entities)};
      false ->
        State#state{entities = #{EntityName => EncodedEntities2}}
    end,
  {reply, Attrs, NewState};
handle_call({list, EntityName}, _From, State = #state{dir = Dir}) ->
  Dir2 = filename:join([Dir, EntityName]),
  case file:list_dir(Dir2) of
    {ok, Filenames} ->
      SortedFilenames = lists:sort(Filenames),
      FlushedEntities = read_files(Dir2, SortedFilenames, []),
      {reply, FlushedEntities, State};
    {error, enoent} ->
      {reply, [], State}
  end;
handle_call(delete_all, _From, State = #state{dir = Dir}) ->
  case file:list_dir(Dir) of
    {ok, Filenames} ->
      [delete_entity_dir(filename:join([Dir, EntityDir])) || EntityDir <- Filenames];
    {error, enoent} -> ok
  end,
  {reply, ok, State#state{entities = #{}}}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(flush, State = #state{dir = Dir, entities = Entities}) ->
  flush(Dir, Entities),
  {noreply, State#state{entities = #{}}}.

terminate(_Reason, #state{interval = Interval}) ->
  timer:cancel(Interval).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

flush(Dir, Entities) ->
  [flush_entities(Dir, EntityName, EncodedEntities) ||
    {EntityName, EncodedEntities} <- maps:to_list(Entities)].

flush_entities(Dir, EntityName, EncodedEntities) ->
  Dir2 = filename:join([Dir, EntityName]),
  ok = filelib:ensure_dir(Dir2 ++ "/"),
  SysTime = erlang:system_time(millisecond),
  Filename = filename:join([Dir2, "_" ++ integer_to_list(SysTime)]),
  file:write_file(Filename, EncodedEntities, [write, exclusive]).

read_files(Directory, [Filename | Rest], Acc) ->
  {ok, Content} = file:read_file(filename:join([Directory, Filename])),
  Entities = [jiffy:decode(Line, [return_maps]) ||
               Line <- binary:split(Content, <<"\n">>, [global, trim])],
  read_files(Directory, Rest, Acc ++ lists:reverse(Entities));
read_files(_Directory, [], Acc) ->
  Acc.

delete_entity_dir(Dir) ->
  {ok, Filenames} = file:list_dir(Dir),
  [file:delete(filename:join([Dir, Filename])) || Filename <- Filenames].
