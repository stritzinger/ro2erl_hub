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
-export([set_topic_bandwidth/3]).


%=== TYPES =====================================================================

%% Message types for protocol communication
-type bridge_dispatch_msg() :: {bridge_dispatch, pid(), integer(), term()}.
-type hub_set_topic_bandwidth_msg() :: {hub_set_topic_bandwidth, binary(),
                                        non_neg_integer() | infinity}.

-export_type([bridge_dispatch_msg/0, hub_set_topic_bandwidth_msg/0]).


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

-doc """
Sets the bandwidth limit for a topic.

Sends a message to the bridge to update the bandwidth limit for a specific topic.

### Parameters:
- BridgePid: pid() - Process ID of the bridge service
- TopicName: binary() - Name of the topic to set bandwidth for
- Bandwidth: non_neg_integer() | infinity - Bandwidth limit in bytes/s, or infinity for no limit

### Example:
```
> ro2erl_hub_bridge:set_topic_bandwidth(BridgePid, <<"/sensor">>, 1024).
ok
> ro2erl_hub_bridge:set_topic_bandwidth(BridgePid, <<"/control">>, infinity).
ok
```
""".
-spec set_topic_bandwidth(BridgePid :: pid(), TopicName :: binary(), Bandwidth :: non_neg_integer() | infinity) -> ok.
set_topic_bandwidth(BridgePid, TopicName, Bandwidth)
  when is_pid(BridgePid), is_binary(TopicName),
       (is_integer(Bandwidth) andalso Bandwidth >= 0) orelse Bandwidth =:= infinity ->
    gen_statem:cast(BridgePid, {hub_set_topic_bandwidth, TopicName, Bandwidth}),
    ok.
