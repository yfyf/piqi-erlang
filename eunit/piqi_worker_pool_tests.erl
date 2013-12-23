-module(piqi_worker_pool_tests).

-include_lib("eunit/include/eunit.hrl").

worker_pool_test_() ->
    {setup,
        fun() -> {ok, _} = piqi_worker_pool:start_link() end,
        fun(_) -> piqi_worker_pool:stop() end,
        [
            {"empty initial list", ?_assertEqual([], piqi_worker_pool:list_workers())},
            {"register workers", ?_assertEqual(true, check_registered())},
            {"unregister on request", ?_assertEqual([], check_unregistered_req())},
            {"unregister on terminate", ?_assertEqual([], check_unregistered_term())},
            {"get no worker", ?_assertEqual({error, no_workers}, piqi_worker_pool:get_worker())},
            {"get single worker", ?_assertMatch(true, check_get_with_single_worker())},
            {"get multiple workers", ?_assertMatch(true, check_get_with_multiple_workers())}
        ]
    }.

check_registered() ->
    Pids = gen_workers(10),
    [piqi_worker_pool:add_worker(P) || P <- Pids],
    Workers = piqi_worker_pool:list_workers(),
    R = lists:all(fun(P) -> lists:member(P, Workers) end, Pids),
    stop_workers(Pids),
    R.

check_unregistered_req() ->
    Pids = gen_workers(10),
    [piqi_worker_pool:add_worker(P) || P <- Pids],
    [piqi_worker_pool:remove_worker(P) || P <- Pids],
    timer:sleep(100), % defer to let piqi_worker_pool do it's job.
    L = piqi_worker_pool:list_workers(),
    stop_workers(Pids),
    L.

check_unregistered_term() ->
    Pids = gen_workers(10),
    [piqi_worker_pool:add_worker(P) || P <- Pids],
    stop_workers(Pids),
    piqi_worker_pool:list_workers().

check_get_with_single_worker() ->
    [Pid] = gen_workers(1),
    piqi_worker_pool:add_worker(Pid),
    Worker = piqi_worker_pool:get_worker(),
    stop_workers([Pid]),
    case Worker of
        {ok, Pid} -> true;
        _ -> false
    end.

check_get_with_multiple_workers() ->
    Pids = gen_workers(10),
    [piqi_worker_pool:add_worker(P) || P <- Pids],
    R = [
        begin
            {ok, P} = piqi_worker_pool:get_worker(),
            P
        end
        || _ <- lists:seq(1,1000)
    ],
    stop_workers(Pids),
    D = lists:foldl(
        fun(Pid, D)->
            dict:update_counter(Pid, 1, D)
        end,
        dict:new(),
        R
    ),
    % can't say much about the distribution on a short run,
    % but at least all workers should have been hit once
    true = length(dict:fetch_keys(D)) =:= length(Pids).

gen_workers(N) ->
    [waiter() || _I <- lists:seq(1, N)].

stop_workers(Pids) ->
    [P ! stop || P <- Pids],
    timer:sleep(100).

waiter() ->
    spawn(
        fun() ->
            receive
                _ -> true
            after
                60000 -> true
            end
        end
    ).

