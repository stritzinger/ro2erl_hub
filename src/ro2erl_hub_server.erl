%% -*- mode: erlang -*-

-module(ro2erl_hub_server).

-moduledoc """
Main hub server

This module is responsible for:
1. Managing bridge connections
2. Processing messages from bridges
3. Forwarding messages between bridges
4. Maintaining bridge state
""".

-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").


%=== EXPORTS ===================================================================

%% API functions
-export([start_link/2]).
-export([get_bridges/0]).
-export([dispatch/1]).

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
    bridge_id :: binary()
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
handle_common({call, From}, get_bridges, _StateName, #data{bridges = Bridges}) ->
    BridgeIds = [BridgeId || #bridge{bridge_id = BridgeId} <- maps:values(Bridges)],
    {keep_state_and_data, [{reply, From, lists:sort(BridgeIds)}]};
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

            % Add bridge to map
            NewBridges = maps:put(BridgePid, #bridge{
                mon_ref = BridgeMon,
                bridge_id = BridgeId
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

            % Update bridge map
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
