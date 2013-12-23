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

% OTP supervisor callbacks
-export([init/1]).


-define(SUPERVISOR, ?MODULE).

start_link(WorkerPoolSize) ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, [WorkerPoolSize]).

init([WorkerPoolSize]) ->
    PiqiServers = [ piqi_server_spec(I) || I <- lists:seq(1, WorkerPoolSize) ],
    {ok, {{one_for_one, 10, 1}, [piqi_workerpool_spec() | PiqiServers]}}.

piqi_workerpool_spec() ->
    {
        piqi_worker_pool,
        {piqi_worker_pool, start_link, []},
        permanent, 1000, worker,
        [piqi_worker_pool]
    }.
piqi_server_spec(N) ->
    {{piqi_tools, N},
        {piqi_tools, start_link, []},
        permanent, 1000, worker,
        [piqi_tools]
    }.
