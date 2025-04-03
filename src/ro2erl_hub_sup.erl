%% -*- mode: erlang -*-

-module(ro2erl_hub_sup).

-moduledoc """
Supervisor module for ro2erl_hub.

This module implements the supervisor behaviour and is responsible for managing
the hub server process and the pg scope.
""".

-behaviour(supervisor).


%=== EXPORTS ===================================================================

%% API functions
-export([start_link/3]).

%% Behaviour supervisor callback functions
-export([init/1]).

-define(SERVER, ?MODULE).


%=== API FUNCTIONS =============================================================

-doc """
Starts the supervisor with the specified process group configuration.

### Parameters:
- PgScope :: atom() - The process group scope to use for all groups
- BridgePgGroup :: atom() - The process group name for bridge discovery
- NotificationPgGroup :: atom() - The process group name for WebSocket notifications

### Returns:
- {ok, Pid} - Supervisor process ID
- {error, Reason} - If supervisor could not be started
""".
-spec start_link(PgScope :: atom(), BridgePgGroup :: atom(), NotificationPgGroup :: atom()) ->
    {ok, pid()} | {error, term()}.
start_link(PgScope, BridgePgGroup, NotificationPgGroup) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [PgScope, BridgePgGroup, NotificationPgGroup]).


%=== BEHAVIOUR supervisor CALLBACK FUNCTIONS =====================================

init([PgScope, BridgePgGroup, NotificationPgGroup]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 10,
        period => 1
    },

    % Start process group scope (only one for all process groups)
    PgScopeSpec = #{
        id => pg_scope,
        start => {pg, start_link, [PgScope]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [pg]
    },

    % Start hub server
    HubServer = #{
        id => ro2erl_hub_server,
        start => {ro2erl_hub_server, start_link, [
            {PgScope, BridgePgGroup},
            {PgScope, NotificationPgGroup},
            ro2erl_hub_bridge
        ]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [ro2erl_hub_server]
    },

    {ok, {SupFlags, [PgScopeSpec, HubServer]}}.
