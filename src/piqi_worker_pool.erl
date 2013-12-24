-module(piqi_worker_pool).

-behaviour(gen_server).

-define(ETS_WORKER_TABLE_NAME, piqi_workers).

%% API
-export([start_link/0, stop/0, get_worker/0, add_worker/1, remove_worker/1, list_workers/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec list_workers() -> [pid()].
list_workers() ->
    case ets:lookup(?ETS_WORKER_TABLE_NAME, workers) of
        [] -> [];
        [{workers, []}] -> [];
        [{workers, W}] -> W;
        undefined -> []
    end.

%% @doc Retrieve a "random" worker
-spec get_worker() -> {'error','no_workers'} | {'ok',pid()}.
get_worker() ->
    case list_workers() of
        [] ->
            {error, no_workers};
        [SingleWorker] ->
            {ok, SingleWorker};
        MultipleWorkers  ->
             {_,_,Usec} = os:timestamp(),
            Index = (Usec rem length(MultipleWorkers)) + 1,
            {ok, lists:nth(Index, MultipleWorkers)}
    end.

-spec add_worker(pid()) -> ok.
add_worker(WorkerPid) ->
    gen_server:call(?MODULE, {add_worker, WorkerPid}),
    ok.

-spec remove_worker(pid()) -> ok.
remove_worker(WorkerPid) ->
    gen_server:cast(?MODULE, {remove_worker, WorkerPid}).

-spec start_link() -> 'ignore' | {'error',_} | {'ok', pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

%% @private
-spec init(_) -> {'ok', state}.
init(_) ->
    create_ets_table(),
    {ok, state}.

%% @private
-spec handle_call(_,_,_) -> {'noreply',_} | {'reply',_,_}.
handle_call({add_worker, Pid}, _From, State) ->
    do_add_worker(Pid),
    {reply, ok, State};
handle_call(stop, _From, State) ->
    {stop, normal, stopping, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
-spec handle_cast(_,_) -> {'noreply',_}.
handle_cast({remove_worker, Pid}, State) ->
    do_remove_worker(Pid),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(_,_) -> {'noreply',_}.
handle_info({'DOWN', _MonitorRef, process, Pid, _}, State) ->
    do_remove_worker(Pid),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(_,_) -> 'ok'.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

%% @private
-spec create_ets_table() -> ets:tid() | atom().
create_ets_table() ->
    ets:new(?ETS_WORKER_TABLE_NAME, [named_table, set, protected, {read_concurrency, true}]),
    ets:insert(?ETS_WORKER_TABLE_NAME, {workers, []}).

%% @private
-spec do_add_worker( pid() ) -> ok.
do_add_worker(Pid) ->
    Current = list_workers(),
    case lists:member(Pid, Current) of
        true -> ok;
        false ->
            do_monitor(Pid),
            put_workers([Pid | Current])
    end,
    ok.

%% @private
-spec do_remove_worker(pid()) -> ok.
do_remove_worker(Pid) ->
    New = [Worker || Worker <- list_workers(), Worker =/= Pid],
    put_workers(New),
    do_demonitor(Pid),
    ok.

%% @private
-spec put_workers([pid()]) -> ok.
put_workers(Workers) ->
    ets:insert(?ETS_WORKER_TABLE_NAME, {workers, Workers}),
    ok.

%% @private
-spec do_monitor(pid()) -> ok.
do_monitor(Pid) ->
    Ref = erlang:monitor(process, Pid),
    ets:insert(?ETS_WORKER_TABLE_NAME, {{ref, Pid}, Ref}),
    ok.

%% @private
-spec do_demonitor(pid()) -> ok.
do_demonitor(Pid) ->
    case ets:lookup(?ETS_WORKER_TABLE_NAME, {ref, Pid}) of
        [{{ref, Pid}, Ref}] ->
            true = erlang:demonitor(Ref, [flush]),
            true = ets:delete(?ETS_WORKER_TABLE_NAME, {ref, Pid}),
            ok;
        _ ->
            ok
    end.
