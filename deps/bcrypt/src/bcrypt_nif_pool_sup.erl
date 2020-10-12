%% Copyright (c) 2011 Hunter Morris
%% Distributed under the MIT license; see LICENSE for details.
-module(bcrypt_nif_pool_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
start_child() -> supervisor:start_child(?MODULE, []).

init([]) ->
    {ok, PoolSize} = application:get_env(bcrypt, nif_pool_size),
    {ok, MaxOverFlow} = application:get_env(bcrypt, nif_pool_max_overflow),

    PoolArgs = [
        {name, {local, bcrypt_nif_pool}},
        {nif_pool_size, PoolSize},
        {nif_pool_max_overflow, MaxOverFlow},
        {worker_module, bcrypt_nif_worker}
    ],

    PoolSpecs = [
        poolboy:child_spec(bcrypt_nif_pool, PoolArgs, [])
    ],

    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.