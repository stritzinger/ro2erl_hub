%% -*- mode: erlang -*-

-module(ro2erl_hub_sup).

-moduledoc """
Supervisor module for ro2erl_hub.

This module implements the supervisor behaviour and is responsible for managing
the hub server process.
""".

-behaviour(supervisor).


%=== EXPORTS ===================================================================

%% API functions
-export([start_link/0]).

%% Behaviour supervisor callback functions
-export([init/1]).

-define(SERVER, ?MODULE).


%=== API FUNCTIONS =============================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%=== BEHAVIOUR supervisor CALLBACK FUNCTIONS =====================================

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 10,
        period => 1
    },

    TargetXPgScope = #{
        id => targetx_pg_scope,
        start => {pg, start_link, [targetx_pg_scope]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [pg]
    },

    HubServer = #{
        id => ro2erl_hub_server,
        start => {ro2erl_hub_server, start_link, [
            {targetx_pg_scope, ro2erl_hub_server},
            ro2erl_hub_bridge
        ]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [ro2erl_hub_server]
    },

    {ok, {SupFlags, [TargetXPgScope, HubServer]}}.
