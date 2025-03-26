%% -*- mode: erlang -*-

-module(ro2erl_hub_bridge).

-moduledoc """
Bridge communication abstraction for ro2erl_hub

This module provides an abstraction layer for communicating with a ro2erl_bridge
service running on a different Erlang node. Since the hub doesn't have
access to the ro2erl_bridge application, this module uses gen_statem calls
to communicate with the bridge.
""".

%% API functions
-export([dispatch/3]).


%=== TYPES =====================================================================

%% Message types for protocol communication
-type bridge_dispatch_msg() :: {bridge_dispatch, pid(), integer(), term()}.

-export_type([bridge_dispatch_msg/0]).


%=== API FUNCTIONS =============================================================

-doc """
Dispatches a message to the bridge

Sends a message to the bridge for local distribution.

### Parameters:
- BridgePid: pid() - Process ID of the bridge service
- Timestamp: integer() - When the message was sent (in milliseconds)
- Message: term() - The message to dispatch

### Example:
```
> ro2erl_hub_bridge:dispatch(BridgePid, erlang:system_time(millisecond), {topic, "/sensor", data}).
ok
```
""".
-spec dispatch(BridgePid :: pid(), Timestamp :: integer(), Message :: term()) -> ok.
dispatch(BridgePid, Timestamp, Message) when is_pid(BridgePid), is_integer(Timestamp) ->
    gen_statem:cast(BridgePid, {hub_dispatch, Timestamp, Message}),
    ok.
