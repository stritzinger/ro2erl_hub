%% -*- mode: erlang -*-

-module(ro2erl_hub_ws_handler).

-moduledoc """
WebSocket handler for ro2erl_hub.

This module implements the cowboy_websocket behaviour and provides a WebSocket endpoint
for the ro2erl_hub frontend. It handles:
1. WebSocket connection management with authentication
2. JSON-RPC 2.0 request/response protocol
3. Real-time notifications via process groups
4. Topic state caching for efficient updates
5. Connection health monitoring via ping/pong
""".

-behaviour(cowboy_websocket).

-include_lib("kernel/include/logger.hrl").


%=== EXPORTS ===================================================================

%% Behaviour cowboy_websocket callback functions
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).


%=== TYPES =====================================================================

-record(state, {
    server_mod :: module(),           % Module to use for server operations
    pg_scope :: atom(),               % Process group scope
    pg_group :: atom(),               % Process group name
    auth_token :: binary(),           % Authentication token
    ping_interval :: integer(),       % Interval between ping frames (ms)
    ping_timeout :: integer(),        % Timeout for pong responses (ms)
    topic_cache = #{} :: #{binary() => map()}, % Cache of topic states
    ping_timer :: reference() | undefined, % Timer for sending periodic pings
    pong_timeout_timer :: reference() | undefined % Timer for pong response timeout
}).

%=== BEHAVIOUR cowboy_websocket CALLBACK FUNCTIONS ==============================

init(Req, #{server_mod := ServerMod,
            ws_pg_info := {PgScope, PgGroup},
            auth_token := AuthToken,
            ping_interval := PingInterval,
            ping_timeout := PingTimeout}) ->
    % Extract token from query parameters
    case cowboy_req:parse_qs(Req) of
        [{<<"token">>, Token}] ->
            case Token =:= AuthToken of
                true ->
                    % Token matches, proceed with WebSocket upgrade
                    {cowboy_websocket, Req, #state{
                        server_mod = ServerMod,
                        pg_scope = PgScope,
                        pg_group = PgGroup,
                        auth_token = AuthToken,
                        ping_interval = PingInterval,
                        ping_timeout = PingTimeout
                    }};
                false ->
                    % Invalid token, return 401
                    Req2 = cowboy_req:reply(401, #{}, <<"Invalid token">>, Req),
                    {ok, Req2, undefined}
            end;
        _ ->
            % No token provided, return 400
            Req2 = cowboy_req:reply(400, #{}, <<"Missing token">>, Req),
            {ok, Req2, undefined}
    end.

websocket_init(State = #state{pg_scope = PgScope, pg_group = PgGroup}) ->
    % Join the notification process group
    case pg:join(PgScope, PgGroup, self()) of
        ok ->
            % Schedule the first ping
            {[], schedule_ping_timer(State)};
        {error, Reason} ->
            ?LOG_ERROR("Failed to join process group: ~p", [Reason]),
            {[{close, 1011, <<"Internal Error">>}], State}
    end.

websocket_handle({text, Data}, State) ->
    % Parse and handle JSON-RPC message
    handle_jsonrpc(Data, State);
websocket_handle(ping, State) ->
    % Cowboy automatically responds with pong
    {[], State};
websocket_handle({ping, _}, State) ->
    % Cowboy automatically responds with pong
    {[], State};
websocket_handle(pong, State) ->
    % Cancel pong timeout timer on receiving pong
    {[], cancel_pong_timeout_timer(State)};
websocket_handle(_Frame, State) ->
    % Ignore other frame types
    ?LOG_DEBUG("Unknown frame: ~p", [_Frame]),
    {[], State}.

websocket_info(send_ping, State) ->
    % Time to send a ping frame to check connection health

    % Schedule a pong timeout if we don't already have one waiting
    {NewState, ShouldSendPing} = schedule_pong_timeout_timer(State),

    % Always reschedule the next ping timer
    FinalState = schedule_ping_timer(NewState),

    % Send the ping frame if appropriate
    case ShouldSendPing of
        true ->
            % We scheduled a new timeout, so send the ping
            {[ping], FinalState};
        false ->
            % We didn't schedule a new timeout, so don't send a ping
            {[], FinalState}
    end;
websocket_info(pong_timeout, State = #state{pong_timeout_timer = TimerRef}) ->
    % Check if this is the current timer to avoid stale timeouts
    case TimerRef of
        undefined ->
            % Stale timeout message, ignore
            {[], State};
        _ ->
            % Current timer expired - client didn't respond with pong
            {[{close, 3008, <<"Ping Timeout">>}], State}
    end;
websocket_info({hub_notification, Notification}, State) ->
    % Handle notification from process group
    handle_hub_notification(Notification, State);
websocket_info(_Info, State) ->
    % Ignore other info messages
    {[], State}.

%=== INTERNAL FUNCTIONS ========================================================

% Schedule a timer to send a ping frame after ping_interval
schedule_ping_timer(State = #state{ping_interval = Interval,
                                   ping_timer = undefined}) ->
    % Schedule new ping timer
    State#state{ping_timer = erlang:send_after(Interval, self(), send_ping)};
schedule_ping_timer(State = #state{ping_timer = OldTimer}) ->
    % Cancel any existing ping timer
    erlang:cancel_timer(OldTimer),
    schedule_ping_timer(State#state{ping_timer = undefined}).

% Schedule a timeout for expecting a pong response, if not already scheduled
% Returns {NewState, WasScheduled} where WasScheduled indicates if a new timer was set
schedule_pong_timeout_timer(State = #state{ping_timeout = Timeout,
                                           pong_timeout_timer = ExistingTimer}) ->
    case ExistingTimer of
        undefined ->
            % No existing timer, schedule a new one
            NewTimer = erlang:send_after(Timeout, self(), pong_timeout),
            {State#state{pong_timeout_timer = NewTimer}, true};
        _Timer ->
            % Timer already exists, don't schedule a new one
            {State, false}
    end.

% Cancel pong timeout timer if active
cancel_pong_timeout_timer(State = #state{pong_timeout_timer = undefined}) ->
    State;
cancel_pong_timeout_timer(State = #state{pong_timeout_timer = Timer}) ->
    erlang:cancel_timer(Timer),
    State#state{pong_timeout_timer = undefined}.

handle_jsonrpc(Data, State) ->
    % Parse JSON with atom key conversion
    case json_decode(Data) of
        {ok, #{jsonrpc := <<"2.0">>, id := Id, method := Method} = Request}
          when is_integer(Id); is_binary(Id) ->
            % Handle JSON-RPC request
            handle_request(Method, maps:get(params, Request, #{}), Id, State);
        {ok, #{jsonrpc := <<"2.0">>, id := Id, result := _Result} = _Response}
          when is_integer(Id); is_binary(Id) ->
            % Ignore responses from client
            {[], State};
        {ok, #{jsonrpc := <<"2.0">>, id := Id, error := _} = _Response}
          when is_integer(Id); is_binary(Id); Id =:= null ->
            % Ignore client errors
            {[], State};
        {ok, #{jsonrpc := <<"2.0">>, params := _} = _Notification} ->
            % Ignore client notifications
            {[], State};
        {ok, InvalidMsg} ->
            % Invalid JSON-RPC structure
            ErrorId = case is_map(InvalidMsg) of
                true ->
                    case maps:get(id, InvalidMsg, null) of
                        Id when is_integer(Id); is_binary(Id) -> Id;
                        _ -> null
                    end;
                false -> null
            end,
            send_error_response(ErrorId, -32600, <<"Invalid Request">>,
                <<"Message did not conform to JSON-RPC 2.0 specification">>, State);
        {error, Reason} ->
            % Send parse error response
            send_error_response(null, -32700, <<"Parse error">>, Reason, State)
    end.

handle_request(Method, Params, Id, State)
  when is_map(Params); is_list(Params) ->
    % Handle different JSON-RPC methods
    Result = case Method of
        <<"topic.get">> ->
            handle_topic_get(Params, State);
        <<"topic.list">> ->
            handle_topic_list(State);
        <<"topic.setBandwidth">> ->
            handle_topic_set_bandwidth(Params, State);
        <<"bridge.list">> ->
            handle_bridge_list(State);
        _ ->
            {error, -32601, <<"Method not found">>, null, State}
    end,

    % Process result and send appropriate response
    case Result of
        {ok, Response, NewState} ->
            send_success_response(Id, Response, NewState);
        {error, Code, Message, Data, NewState} ->
            send_error_response(Id, Code, Message, Data, NewState)
    end;
handle_request(_Method, _Params, Id, State) ->
    % Invalid parameters
    send_error_response(Id, -32602, <<"Invalid params">>, null, State).

handle_topic_get(#{topic_name := TopicName},
                 State = #state{server_mod = ServerMod, topic_cache = Cache}) ->
    case ServerMod:get_topic(TopicName) of
        {ok, TopicInfo} ->
            % Add to cache (keep full version in cache)
            NewCache = Cache#{TopicName => TopicInfo},
            % Sanitize for client
            ClientTopic = sanitize_topic_for_client(TopicInfo),
            {ok, ClientTopic, State#state{topic_cache = NewCache}};
        {error, not_found} ->
            {error, -32001, <<"Topic not found">>, null, State};
        {error, Reason} ->
            {error, -32603, <<"Internal error">>, Reason, State}
    end;
handle_topic_get(_Params, State) ->
    % Invalid parameters
    {error, -32602, <<"Invalid params">>, null, State}.

handle_topic_list(State = #state{server_mod = ServerMod, topic_cache = Cache0}) ->
    Topics = ServerMod:get_topics(),
    % Update cache and build client topic list in a single fold
    {NewCache, ClientTopics} =
        maps:fold(fun(TopicName, TopicInfo, {Cache, ClientTopics}) ->
            ClientTopic = sanitize_topic_for_client(TopicInfo),
            {Cache#{TopicName => TopicInfo}, [ClientTopic | ClientTopics]}
        end, {Cache0, []}, Topics),

    % Return the client topics in the original order
    {ok, lists:reverse(ClientTopics), State#state{topic_cache = NewCache}}.

handle_topic_set_bandwidth(#{bandwidth := Bandwidth}, State)
  when Bandwidth =/= null, not is_integer(Bandwidth) orelse Bandwidth =< 0 ->
    % Return invalid bandwidth error for negative or zero values
    {error, -32003, <<"Invalid bandwidth">>, <<"Bandwidth must be a positive number">>, State};
handle_topic_set_bandwidth(#{topic_name := TopicName,
                             bandwidth := BandwidthParams},
                           State = #state{server_mod = ServerMod}) ->
    Bandwidth = case BandwidthParams of
        null -> infinity;
        _ -> BandwidthParams
    end,
    case ServerMod:set_topic_bandwidth(TopicName, Bandwidth) of
        ok ->
            % No need to refresh the topic, the server will send a notification
            % that will be handled by handle_hub_notification
            {ok, <<"ok">>, State};
        {error, not_found} ->
            {error, -32001, <<"Topic not found">>, null, State};
        {error, not_filterable} ->
            {error, -32002, <<"Topic not filterable">>, null, State};
        {error, Reason} ->
            {error, -32603, <<"Internal error">>, Reason, State}
    end;
handle_topic_set_bandwidth(_Params, State) ->
    % Invalid parameters
    {error, -32602, <<"Invalid params">>, null, State}.

handle_bridge_list(State = #state{server_mod = ServerMod}) ->
    Bridges = ServerMod:get_bridges(),
    {ok, Bridges, State}.

handle_hub_notification(Notification, State = #state{topic_cache = Cache}) ->
    % Format notification as JSON-RPC
    case Notification of
        {topic_updated, Topic = #{topic_name := TopicName}} ->
            % Check if topic state has changed
            case has_topic_changed(Topic, maps:get(TopicName, Cache, #{})) of
                true ->
                    % Filter out PIDs and other internal details from the topic before sending
                    % to the client, while keeping the full topic for our cache
                    ClientTopic = sanitize_topic_for_client(Topic),
                    % Update cache
                    NewState = State#state{topic_cache = Cache#{TopicName => Topic}},
                    % Send notification
                    send_notification(<<"topic.updated">>, ClientTopic, NewState);
                false ->
                    % No change, skip notification
                    {[], State}
            end;
        {bridge_attached, BridgeId} ->
            % Send bridge attach notification
            send_notification(<<"bridge.attached">>, #{bridge_id => BridgeId}, State);
        {bridge_detached, BridgeId, Reason} ->
            % Send bridge detach notification with provided reason
            BridgeInfo = #{bridge_id => BridgeId, reason => Reason},
            send_notification(<<"bridge.detached">>, BridgeInfo, State);
        _ ->
            % Ignore unknown notifications
            {[], State}
    end.

%% Filter out PIDs and other internal details from the topic before sending to client
sanitize_topic_for_client(Topic) ->
    % Change infinity to null in bandwidth_limit
    BandwidthLimit = case maps:get(bandwidth_limit, Topic) of
        infinity -> null;
        Limit -> Limit
    end,
    % Keep only the fields needed by the client
    #{
        topic_name => maps:get(topic_name, Topic),
        filterable => maps:get(filterable, Topic),
        bandwidth_limit => BandwidthLimit,
        metrics => maps:get(metrics, Topic)
    }.


%=== JSON-RPC HELPER FUNCTIONS =================================================

json_object_push(Key, Value, Acc) ->
    try
        AtomKey = binary_to_existing_atom(Key, utf8),
        [{AtomKey, Value} | Acc]
    catch
        error:badarg -> % Atom does not exist
            [{Key, Value} | Acc] % Keep as binary
    end.

json_decode(Data) ->
    try json:decode(Data,ok, #{object_push => fun json_object_push/3}) of
        {Result, _Acc, <<>>} ->
            {ok, Result};
        {_Result, _Acc, _Rest} ->
            {error, invalid_json}
    catch
        error:Reason ->
            {error, Reason}
    end.

json_encode(Term) ->
    iolist_to_binary(json:encode(Term)).

send_success_response(Id, Result, State) ->
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => Result
    },
    {[{text, json_encode(Response)}], State}.

send_error_response(Id, Code, Message, Data, State) ->
    % Ensure Data is JSON-encodable
    SafeData = try
        % Try encoding just the data to test if it's valid
        case Data of
            null -> null;
            _ ->
                _ = json_encode(Data),
                Data
        end
    catch
        _:_ ->
            % If encoding fails, convert to binary string
            try
                list_to_binary(io_lib:format("~p", [Data]))
            catch
                _:_ -> <<"non-encodable error data">>
            end
    end,

    Error = #{
        <<"code">> => Code,
        <<"message">> => Message,
        <<"data">> => SafeData
    },
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => Error
    },
    {[{text, json_encode(Response)}], State}.

send_notification(Method, Params, State) ->
    Notification = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method,
        <<"params">> => Params
    },
    {[{text, json_encode(Notification)}], State}.


%=== TOPIC CACHE HELPER FUNCTIONS ==============================================

has_topic_changed(#{topic_name := TopicName, filterable := Filterable,
                    bandwidth_limit := BandwidthLimit, metrics := Metrics},
                  #{topic_name := TopicName, filterable := Filterable,
                    bandwidth_limit := BandwidthLimit, metrics := OldMetrics}) ->
    has_metrics_changed(Metrics, OldMetrics);
has_topic_changed(_Topic1, _Topic2) ->
    true.

%% Helper function to check if any metrics have changed
has_metrics_changed(#{dispatched := Dispatched1, forwarded := Forwarded1},
                    #{dispatched := Dispatched2, forwarded := Forwarded2}) ->
    has_metric_item_changed(Dispatched1, Dispatched2)
        orelse has_metric_item_changed(Forwarded1, Forwarded2);
has_metrics_changed(_Metrics1, _Metrics2) ->
    true.

%% Helper function to check if a specific metric item has changed
has_metric_item_changed(#{bandwidth := Bandwidth1, rate := Rate1},
                        #{bandwidth := Bandwidth2, rate := Rate2}) ->
    RateDelta = 0.01,
    (Bandwidth1 =/= Bandwidth2) orelse (abs(Rate1 - Rate2) > RateDelta);
has_metric_item_changed(_Item1, _Item2) ->
    true.
