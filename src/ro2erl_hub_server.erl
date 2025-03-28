%% -*- mode: erlang -*-

-module(ro2erl_hub_server).

-moduledoc """
Main hub server

This module is responsible for:
1. Managing bridge connections
2. Processing messages from bridges
3. Forwarding messages between bridges
4. Maintaining bridge state
5. Tracking topic information from bridges
""".

-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").


%=== EXPORTS ===================================================================

%% API functions
-export([start_link/2]).
-export([get_bridges/0]).
-export([dispatch/1]).
-export([get_topics/0]).
-export([get_topic/1]).

%% Behaviour gen_statem callback functions
-export([callback_mode/0]).
-export([init/1]).
-export([terminate/3]).
-export([code_change/4]).

%% State functions
-export([idle/3]).
-export([forwarding/3]).


%=== MACROS ====================================================================

-define(SERVER, ?MODULE).


%=== TYPES =====================================================================

-record(bridge, {
    mon_ref :: reference(),
    bridge_id :: binary(),
    topics = #{} :: #{binary() => #{           % Map of topic name to topic info
        filterable := boolean(),               % Whether the topic can be filtered
        bandwidth_limit := non_neg_integer() | infinity, % Current bandwidth limit
        metrics := #{                          % Topic metrics from this bridge
            dispatched := #{bandwidth := non_neg_integer(), rate := float()},
            forwarded := #{bandwidth := non_neg_integer(), rate := float()}
        }
    }}
}).

-record(data, {
    bridges = #{} :: #{pid() => #bridge{}},  % Map of bridge pids to bridge records
    bridge_mod :: module()                   % Module to use for bridge communication
}).


%=== API FUNCTIONS =============================================================

start_link(HubGroup = {_Scope, _Group}, BridgeMod) ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [{HubGroup, BridgeMod}], []).

-doc """
Gets the list of currently attached bridge IDs.

### Returns:
- [BridgeId :: term()] - List of bridge IDs currently attached to the hub
""".
-spec get_bridges() -> [BridgeId :: term()].
get_bridges() ->
    gen_statem:call(?SERVER, get_bridges).

-doc """
Dispatches a message to all attached bridges.

### Parameters:
- Message :: term() - The message to dispatch to all bridges

The message will be sent to all bridges with a timestamp and no specific sender.
""".
-spec dispatch(Message :: term()) -> ok.
dispatch(Message) ->
    Timestamp = erlang:system_time(millisecond),
    gen_statem:cast(?SERVER, {bridge_dispatch, undefined, Timestamp, Message}).

-doc """
Gets information about all known topics, consolidated across all bridges.

### Returns:
- #{TopicName :: binary() => TopicInfo :: map()} - Map of topics to consolidated topic information
""".
-spec get_topics() -> #{binary() => map()}.
get_topics() ->
    gen_statem:call(?SERVER, get_topics).

-doc """
Gets information about a specific topic, consolidated across all bridges.

### Parameters:
- TopicName :: binary() - The name of the topic to retrieve

### Returns:
- {ok, TopicInfo :: map()} - Topic information if topic exists
- {error, not_found} - If the topic doesn't exist
""".
-spec get_topic(TopicName :: binary()) -> {ok, map()} | {error, not_found}.
get_topic(TopicName) ->
    gen_statem:call(?SERVER, {get_topic, TopicName}).


%=== BEHAVIOUR gen_statem CALLBACK FUNCTIONS ===================================

callback_mode() -> [state_functions].

init([{{Scope, Group}, BridgeMod}]) ->
    % Join the process group
    pg:join(Scope, Group, self()),

    % Initialize state
    {ok, idle, #data{
        bridges = #{},
        bridge_mod = BridgeMod
    }}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.


%=== STATE FUNCTIONS ===========================================================

idle(cast, {bridge_dispatch, _Sender, _Timestamp, _Message}, _Data) ->
    ?LOG_WARNING("Cannot forward message: no bridges connected"),
    keep_state_and_data;
idle(cast, {bridge_detach, _BridgePid}, _Data) ->
    % When idle, there are no bridges to detach from
    keep_state_and_data;
idle(EventType, EventContent, Data) ->
    handle_common(EventType, EventContent, ?FUNCTION_NAME, Data).

forwarding(cast, {bridge_detach, BridgePid}, Data) ->
    case detach_from_bridge(BridgePid, Data) of
        {error, not_attached} ->
            keep_state_and_data;
        {ok, NewData} ->
            % If no more bridges, transition to idle state
            case maps:size(NewData#data.bridges) of
                0 -> {next_state, idle, NewData};
                _ -> {keep_state, NewData}
            end
    end;
forwarding(cast, {bridge_dispatch, Sender, Timestamp, Message}, Data) ->
    % Forward message to all bridges except sender
    forward_to_all_bridges(Sender, Timestamp, Message, Data),
    keep_state_and_data;
forwarding(EventType, EventContent, Data) ->
    handle_common(EventType, EventContent, ?FUNCTION_NAME, Data).


%=== COMMON EVENT HANDLING =====================================================

handle_common(cast, {bridge_attach, BridgeId, BridgePid}, StateName, Data) ->
    case attach_bridge(BridgePid, BridgeId, Data) of
        {error, already_attached} ->
            keep_state_and_data;
        {ok, NewData} ->
            % Transition to forwarding state if we're currently idle
            case {StateName, maps:size(Data#data.bridges)} of
                {idle, 0} -> {next_state, forwarding, NewData};
                _ -> {keep_state, NewData}
            end
    end;
handle_common(cast, {bridge_update_topics, BridgePid, BridgeTopics}, _StateName, Data = #data{bridges = Bridges}) ->
    case maps:find(BridgePid, Bridges) of
        error ->
            ?LOG_WARNING("Received topic update from unknown bridge: ~p", [BridgePid]),
            keep_state_and_data;
        {ok, Bridge} ->
            % Update the topics in the bridge record
            NewBridge = Bridge#bridge{topics = BridgeTopics},
            NewBridges = Bridges#{BridgePid => NewBridge},
            NewData = Data#data{bridges = NewBridges},
            {keep_state, NewData}
    end;
handle_common({call, From}, get_bridges, _StateName, #data{bridges = Bridges}) ->
    BridgeIds = [BridgeId || #bridge{bridge_id = BridgeId} <- maps:values(Bridges)],
    {keep_state_and_data, [{reply, From, lists:sort(BridgeIds)}]};
handle_common({call, From}, get_topics, _StateName, #data{bridges = Bridges}) ->
    % Consolidate topics from all bridges on-demand
    ConsolidatedTopics = consolidate_topics(Bridges),
    {keep_state_and_data, [{reply, From, ConsolidatedTopics}]};
handle_common({call, From}, {get_topic, TopicName}, _StateName, #data{bridges = Bridges}) ->
    % Find information about a specific topic across all bridges
    case consolidate_topic(TopicName, Bridges) of
        {ok, TopicInfo} ->
            {keep_state_and_data, [{reply, From, {ok, TopicInfo}}]};
        {error, not_found} ->
            {keep_state_and_data, [{reply, From, {error, not_found}}]}
    end;
handle_common(info, {'DOWN', MonRef, process, Pid, Reason}, StateName,
              Data = #data{bridges = Bridges}) ->
    % Check if this is one of our bridges
    case find_bridge_by_monitor(MonRef, Bridges) of
        {ok, Pid, #bridge{bridge_id = BridgeId}} ->
            % Remove the bridge from our map
            NewBridges = maps:remove(Pid, Bridges),
            NewData = Data#data{bridges = NewBridges},

            ?LOG_NOTICE("Bridge ~p (~p) detached: ~p", [BridgeId, Pid, Reason]),

            % Transition to idle state if there is no more bridges
            case {StateName, maps:size(NewBridges)} of
                {forwarding, 0} -> {next_state, idle, NewData};
                _ -> {keep_state, NewData}
            end;
        _ ->
            % Not our bridge, ignore
            keep_state_and_data
    end;
handle_common(cast, Message, StateName, _Data) ->
    ?LOG_ERROR("Unexpected cast event in state ~p: ~p", [StateName, Message]),
    {stop, {error, {unexpected_cast, Message}}};
handle_common({call, From}, Message, StateName, _Data) ->
    ?LOG_ERROR("Unexpected call event from ~p in state ~p: ~p", [From, StateName, Message]),
    {stop, {error, {unexpected_call, Message}}, [{reply, From, {error, not_supported}}]};
handle_common(info, Message, StateName, _Data) ->
    ?LOG_WARNING("Unexpected info message in state ~p: ~p", [StateName, Message]),
    keep_state_and_data.


%=== INTERNAL FUNCTIONS ========================================================

-spec find_bridge_by_monitor(MonRef :: reference(),
                             Bridges :: #{pid() => #bridge{}}) ->
    {ok, pid(), #bridge{}} | {error, not_found}.
find_bridge_by_monitor(MonRef, Bridges) ->
    % Find bridge that matches the monitor reference
    Result = maps:fold(fun(Pid, #bridge{mon_ref = Ref} = Bridge, Acc) ->
        case Ref =:= MonRef of
            true -> {ok, Pid, Bridge};
            false -> Acc
        end
    end, {error, not_found}, Bridges),
    Result.

-spec attach_bridge(BridgePid :: pid(), BridgeId :: binary(), Data :: #data{}) ->
    {ok, #data{}} | {error, already_attached}.
attach_bridge(BridgePid, BridgeId, Data = #data{bridges = Bridges}) ->
    case maps:is_key(BridgePid, Bridges) of
        true ->
            {error, already_attached};
        false ->
            % Monitor bridge process
            BridgeMon = monitor(process, BridgePid),

            % Create a new bridge record with empty topics map
            NewBridges = maps:put(BridgePid, #bridge{
                mon_ref = BridgeMon,
                bridge_id = BridgeId,
                topics = #{}
            }, Bridges),

            NewData = Data#data{bridges = NewBridges},

            ?LOG_NOTICE("Bridge ~p (~p) attached", [BridgeId, BridgePid]),

            {ok, NewData}
    end.

-spec detach_from_bridge(BridgePid :: pid(), Data :: #data{}) ->
    {ok, #data{}} | {error, not_attached}.
detach_from_bridge(BridgePid, Data = #data{bridges = Bridges}) ->
    case maps:take(BridgePid, Bridges) of
        error ->
            {error, not_attached};
        {#bridge{mon_ref = MonRef, bridge_id = BridgeId}, NewBridges} ->
            % Demonitor bridge process and flush any pending messages
            demonitor(MonRef, [flush]),

            % Update state
            NewData = Data#data{bridges = NewBridges},

            ?LOG_NOTICE("Bridge ~p (~p) detached", [BridgeId, BridgePid]),

            {ok, NewData}
    end.

-spec forward_to_all_bridges(Sender :: pid() | undefined,
                             Timestamp :: integer(),
                             Message :: term(),
                             Data :: #data{}) -> ok.
forward_to_all_bridges(Sender, Timestamp, Message,
                       #data{bridges = Bridges, bridge_mod = BridgeMod}) ->
    % Forward to all bridges except sender
    maps:foreach(fun(BridgePid, #bridge{bridge_id = BridgeId}) ->
        case Sender =:= undefined orelse BridgePid =/= Sender of
            true ->
                ?LOG_DEBUG("Forwarding message from bridge ~p (~p) with timestamp ~p: ~p",
                          [BridgeId, BridgePid, Timestamp, Message]),
                BridgeMod:dispatch(BridgePid, Timestamp, Message);
            false -> ok
        end
    end, Bridges).

-doc """
Consolidates topic information from all bridges.

This function builds a global view of all topics by collecting and merging
information from all bridges.
""".
-spec consolidate_topics(Bridges :: #{pid() => #bridge{}}) -> #{binary() => map()}.
consolidate_topics(Bridges) ->
    % Collect all unique topic names across bridges in a single fold operation
    TopicNameMap = maps:fold(fun(_, #bridge{topics = Topics}, Acc) ->
        maps:fold(fun(K, _, M) -> M#{K => true} end, Acc, Topics)
    end, #{}, Bridges),

    % Convert set to list
    TopicNames = maps:keys(TopicNameMap),

    % Consolidate information for each topic
    lists:foldl(fun(TopicName, Acc) ->
        case consolidate_topic(TopicName, Bridges) of
            {ok, TopicInfo} ->
                Acc#{TopicName => TopicInfo};
            {error, not_found} ->
                Acc
        end
    end, #{}, TopicNames).

-doc """
Consolidates information for a specific topic across all bridges.
""".
-spec consolidate_topic(TopicName :: binary(),
                        Bridges :: #{pid() => #bridge{}}) ->
    {ok, map()} | {error, not_found}.
consolidate_topic(TopicName, Bridges) ->
    % Find all bridges that have this topic
    BridgesWithTopic = [
        {BridgePid, maps:get(TopicName, Bridge#bridge.topics)}
        || {BridgePid, Bridge} <- maps:to_list(Bridges),
           maps:is_key(TopicName, Bridge#bridge.topics)
    ],

    case BridgesWithTopic of
        [] ->
            {error, not_found};
        _ ->
            % Extract bridge PIDs
            BridgePids = [BridgePid || {BridgePid, _} <- BridgesWithTopic],

            % Determine filterability - if any bridge marks it non-filterable, it's non-filterable
            Filterable = lists:all(fun({_, TopicInfo}) ->
                maps:get(filterable, TopicInfo, true)
            end, BridgesWithTopic),

            % Get the minimum bandwidth limit (most restrictive)
            BandwidthLimit = get_min_bandwidth_limit(BridgesWithTopic),

            % Merge metrics from all bridges
            Metrics = merge_all_metrics(BridgesWithTopic),

            % Return consolidated topic information
            {ok, #{
                bridge_pids => BridgePids,
                filterable => Filterable,
                bandwidth_limit => BandwidthLimit,
                metrics => Metrics
            }}
    end.

-doc """
Find the minimum (most restrictive) bandwidth limit across all bridges.
""".
-spec get_min_bandwidth_limit(BridgesWithTopic :: list()) -> non_neg_integer() | infinity.
get_min_bandwidth_limit(BridgesWithTopic) ->
    % Get all bandwidth limits
    BandwidthLimits = [
        maps:get(bandwidth_limit, TopicInfo, infinity)
        || {_, TopicInfo} <- BridgesWithTopic
    ],

    % Find the minimum, considering infinity as the highest possible value
    lists:foldl(fun
        (infinity, infinity) -> infinity;
        (infinity, Min) -> Min;
        (Limit, infinity) -> Limit;
        (Limit, Min) when Limit < Min -> Limit;
        (_, Min) -> Min
    end, infinity, BandwidthLimits).

-doc """
Merges metrics from all bridges for a specific topic.

This function takes metrics from all bridges that have a specific topic
and merges them into a single metrics map.
""".
-spec merge_all_metrics(BridgesWithTopic :: list()) -> map().
merge_all_metrics(BridgesWithTopic) ->
    % Default metrics structure
    DefaultMetrics = #{
        dispatched => #{bandwidth => 0, rate => 0.0},
        forwarded => #{bandwidth => 0, rate => 0.0}
    },

    % Extract metrics from all bridges
    AllMetrics = [
        maps:get(metrics, TopicInfo, DefaultMetrics)
        || {_, TopicInfo} <- BridgesWithTopic
    ],

    % Fold all metrics into a single consolidated view
    lists:foldl(fun(Metrics, AccMetrics) ->
        merge_metrics(Metrics, AccMetrics)
    end, DefaultMetrics, AllMetrics).

-spec merge_metrics(NewMetrics :: map(), CurrentMetrics :: map()) -> map().
merge_metrics(NewMetrics, CurrentMetrics) ->
    % Default metrics structure
    DefaultMetric = #{bandwidth => 0, rate => 0.0},

    % Get dispatched metrics
    NewDispatched = maps:get(dispatched, NewMetrics, DefaultMetric),
    CurrentDispatched = maps:get(dispatched, CurrentMetrics, DefaultMetric),

    % Get forwarded metrics
    NewForwarded = maps:get(forwarded, NewMetrics, DefaultMetric),
    CurrentForwarded = maps:get(forwarded, CurrentMetrics, DefaultMetric),

    % Merge dispatched metrics - sum both bandwidth and rates
    MergedDispatched = #{
        bandwidth => maps:get(bandwidth, NewDispatched, 0) +
                     maps:get(bandwidth, CurrentDispatched, 0),
        rate => maps:get(rate, NewDispatched, 0.0) +
                maps:get(rate, CurrentDispatched, 0.0)
    },

    % Merge forwarded metrics - same approach
    MergedForwarded = #{
        bandwidth => maps:get(bandwidth, NewForwarded, 0) +
                     maps:get(bandwidth, CurrentForwarded, 0),
        rate => maps:get(rate, NewForwarded, 0.0) +
                maps:get(rate, CurrentForwarded, 0.0)
    },

    % Return combined metrics
    #{
        dispatched => MergedDispatched,
        forwarded => MergedForwarded
    }.
