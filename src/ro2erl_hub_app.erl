%% -*- mode: erlang -*-

-module(ro2erl_hub_app).

-moduledoc """
Main application module for ro2erl_hub.

This module implements the OTP application behavior.
""".

-behaviour(application).


%=== EXPORTS ===================================================================

%% Application callbacks
-export([start/2, stop/1]).


%=== BEHAVIOUR application CALLBACK FUNCTIONS ==================================

-doc """
Starts the Target-X Hub application.

### Parameters:
- Type :: term() - The type of start operation
- Args :: term() - The application start arguments

### Returns:
- {ok, Pid} - Supervisor process ID
- {error, Reason} - If application could not be started
""".
-spec start(Type :: term(), Args :: term()) -> {ok, pid()} | {error, term()}.
start(_Type, _Args) ->
    % Get the auth token from configuration
    {ok, WebAuthToken} = application:get_env(ro2erl_hub, web_auth_token),

    % Set up process group configuration
    PgScope = targetx_pg_scope,
    BridgePgGroup = ro2erl_hub_server,
    NotificationPgGroup = ws_notifications,

    % Get WebSocket ping/pong settings from configuration
    {ok, WsPingInterval} = application:get_env(ro2erl_hub, ws_ping_interval),
    {ok, WsPingTimeout} = application:get_env(ro2erl_hub, ws_ping_timeout),

    % Get HTTP port from configuration
    {ok, Port} = application:get_env(ro2erl_hub, port),

    % Start supervisor
    {ok, Pid} = ro2erl_hub_sup:start_link(PgScope, BridgePgGroup, NotificationPgGroup),

    % Configure Cowboy routes for WebSockets
    WebSocketDispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, ro2erl_hub, "web/static/index.html"}},
            {"/ws", ro2erl_hub_ws_handler, #{
                server_mod => ro2erl_hub_server,
                ws_pg_info => {PgScope, NotificationPgGroup},
                auth_token => WebAuthToken,
                ping_interval => WsPingInterval,
                ping_timeout => WsPingTimeout
            }},
            {"/[...]", cowboy_static, {priv_dir, ro2erl_hub, ["web/static"], [{mimetypes, cow_mimetypes, all}]}}
        ]}
    ]),
    io:format("Port: ~p~n", [Port]),
    % Start HTTP server (Cowboy)
    {ok, _} = cowboy:start_clear(
        http,
        [{port, Port}],
        #{env => #{dispatch => WebSocketDispatch}}
    ),

    {ok, Pid}.

-doc """
Stops the RO2 Erlang Hub application.

### Parameters:
- State :: term() - The application state

### Returns:
- ok
""".
-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok = cowboy:stop_listener(http),
    ok.
