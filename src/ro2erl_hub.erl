-module(ro2erl_hub).

-moduledoc """
ro2erl_hub public API

This module provides a generic public API for interacting with the ro2erl_hub system.
It delegates calls to the underlying ro2erl_hub_server process.
""".

%% API functions
-export([get_bridges/0]).
-export([get_topics/0]).
-export([get_topic/1]).
-export([set_topic_bandwidth/2]).


%=== API FUNCTIONS =============================================================

-doc """
Gets the list of currently attached bridge IDs.
""".
-spec get_bridges() -> [BridgeId :: term()].
get_bridges() ->
    ro2erl_hub_server:get_bridges().

-doc """
Gets information about all known topics, consolidated across all bridges.
""".
-spec get_topics() -> #{binary() => map()}.
get_topics() ->
    ro2erl_hub_server:get_topics().

-doc """
Gets information about a specific topic, consolidated across all bridges.
""".
-spec get_topic(TopicName :: binary()) -> {ok, map()} | {error, not_found}.
get_topic(TopicName) ->
    ro2erl_hub_server:get_topic(TopicName).

-doc """
Sets the bandwidth limit for a specific topic across all relevant bridges.

Note: Use `Bandwidth = infinity` to remove the bandwidth limit.
""".
-spec set_topic_bandwidth(TopicName :: binary(), Bandwidth :: non_neg_integer() | infinity) ->
    ok | {error, not_found | not_filterable}.
set_topic_bandwidth(TopicName, Bandwidth) ->
    ro2erl_hub_server:set_topic_bandwidth(TopicName, Bandwidth).
