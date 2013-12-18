%% Copyright 2009, 2010, 2011, 2012, 2013 Anton Lavrik
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%
%% @doc OTP supervisor behavior for Piqi
%%
-module(piqi_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([get_worker/0, remove_worker/1]).

% OTP supervisor callbacks
-export([init/1]).


-define(SUPERVISOR, ?MODULE).

start_link(WorkerPoolSize) ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, [WorkerPoolSize]).

get_worker() ->
    case pg2:get_members(piqi_workers) of
        [] ->
            {error, no_workers};
        [SingleWorker] ->
            {ok, SingleWorker};
        MultipleWorkers ->
            {_,_,Usec} = os:timestamp(),
            Index = (Usec rem length(MultipleWorkers)) + 1,
            {ok, lists:nth(Index, MultipleWorkers)}
    end.

remove_worker(Pid) ->
    pg2:leave(piqi_workers, Pid).

init([WorkerPoolSize]) ->
    pg2:create(piqi_workers),
    PiqiServers = [ piqi_server_spec(I) || I <- lists:seq(1, WorkerPoolSize) ],
    {ok, {{one_for_one, 10, 1}, PiqiServers}}.

piqi_server_spec(N) ->
    {{piqi_tools, N},
        {piqi_tools, start_link, []},
        permanent, 1000, worker,
        [piqi_tools]
    }.
