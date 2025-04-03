-module(ro2erl_hub_ws_handler_SUITE).

-moduledoc """
Test suite for ro2erl_hub_ws_handler

This test suite tests the WebSocket handler functionality including:
- WebSocket connection management with authentication
- JSON-RPC 2.0 request/response handling
- Real-time notifications via process groups
- Connection error handling
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").


%=== EXPORTS ===================================================================

%% Test exports
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    % Authentication tests
    auth_success_test/1,
    auth_failure_test/1,
    % Basic API tests
    topic_list_test/1,
    topic_get_test/1,
    bridge_list_test/1,
    topic_set_bandwidth_test/1,
    % Notification tests
    topic_update_notification_test/1,
    bridge_notification_test/1,
    % Advanced tests
    multiple_connections_test/1,
    connection_cleanup_test/1,
    % Error handling tests
    invalid_request_test/1,
    invalid_method_test/1,
    invalid_params_test/1
]).

%% Server callbacks - Used by the WebSocket handler
-export([
    get_topics/0,
    get_topic/1,
    get_bridges/0,
    set_topic_bandwidth/2
]).


%=== MACROS ===================================================================

%% Test constants
-define(TEST_PORT, 8082).
-define(TEST_TOKEN, <<"test_token">>).
-define(TEST_BRIDGES_TABLE, test_bridges).
-define(TEST_TOPICS_TABLE, test_topics).
-define(PG_SCOPE, test_pg_scope).
-define(PG_GROUP, ws_notification_group).

%% JSON-RPC error codes
-define(JSONRPC_TOPIC_NOT_FOUND, -32001).
-define(JSONRPC_NOT_FILTERABLE, -32002).
-define(JSONRPC_INVALID_BANDWIDTH, -32003).
-define(JSONRPC_PARSE_ERROR, -32700).
-define(JSONRPC_INVALID_REQUEST, -32600).
-define(JSONRPC_METHOD_NOT_FOUND, -32601).
-define(JSONRPC_INVALID_PARAMS, -32602).
-define(JSONRPC_INTERNAL_ERROR, -32603).
-define(JSONRPC_UNAUTHORIZED, -32000).
-define(JSONRPC_INVALID_BRIDGE, -32004).
-define(JSONRPC_MAX_SUBSCRIPTIONS, -32005).


%% Helper macros for assertions

% Asserts that the next received message is a successful JSON-RPC result with EXPECTED_ID and matching EXPECTED_RESULT.
% 1. Waits up to 1000ms for a response matching the EXPECTED_ID.
% 2. Asserts the response is a success ({result: ...}) and the result matches EXPECTED_RESULT.
% 3. Fails on timeout, if an error response is received, or if the result does not match.
% 4. Temporarily stores unrelated messages received during the wait.
% 5. Restores unrelated messages to the process mailbox upon successful assertion.
% 6. Returns the matched result.
% Internal variables use a unique prefix (M_Var) to avoid warnings and namespace
% collisions with variables in the calling function's scope, ensuring macro hygiene.
-define(assertReceiveResult(CLIENT, EXPECTED_ID, EXPECTED_RESULT), fun() ->
    {M_GunPid, M_WsRef} = CLIENT,
    M_TimeoutAt = erlang:system_time(millisecond) + 1000,
    M_WaitJsonRpc = fun M_WaitFun(M_MsgStack) ->
        M_Timeout = M_TimeoutAt - erlang:system_time(millisecond),
        receive
            {gun_ws, M_GunPid, M_WsRef, {text, M_Data}} = M_Msg ->
                case json_decode(M_Data) of
                    #{jsonrpc := <<"2.0">>, id := EXPECTED_ID, result := EXPECTED_RESULT = M_Result} ->
                        {M_Result, M_MsgStack};
                    #{jsonrpc := <<"2.0">>, id := EXPECTED_ID, result := M_UnexpectedResult} ->
                        dump_mailbox(),
                        ct:fail({unexpected_result, ??EXPECTED_RESULT, M_UnexpectedResult,
                                ?FUNCTION_NAME, ?LINE});
                    #{jsonrpc := <<"2.0">>, id := EXPECTED_ID, error := M_Error} ->
                        dump_mailbox(),
                        ct:fail({unexpected_error, M_Error, ?FUNCTION_NAME, ?LINE});
                    #{jsonrpc := <<"2.0">>, id := EXPECTED_ID} = M_Response ->
                        dump_mailbox(),
                        ct:fail({unexpected_response, M_Response, ?FUNCTION_NAME, ?LINE});
                    _OtherJson -> % Catch other valid JSON messages not matching above
                        M_WaitFun([M_Msg | M_MsgStack]) % Put back and keep waiting
                end;
            M_OtherMsg -> % Catch non-gun_ws messages or messages from other connections
                M_WaitFun([M_OtherMsg | M_MsgStack]) % Put back and keep waiting
        after M_Timeout ->
            dump_mailbox(),
            ct:fail({response_timeout, assertReceiveResult, ??EXPECTED_ID, ?FUNCTION_NAME, ?LINE})
        end
    end,
    {M_Result, M_Msgs} = M_WaitJsonRpc([]),
    % Restore unprocessed messages
    lists:foreach(fun(M_Msg) -> self() ! M_Msg end, lists:reverse(M_Msgs)),
    M_Result
end()).

% Sends a request and asserts the successful result matches EXPECTED_RESULT.
% 1. Sends a JSON-RPC request using the CLIENT, METHOD, and PARAMS, obtaining a request ID.
% 2. Calls assertReceiveResult to wait for and validate the success response for that ID.
% 3. Returns the matched result.
% Internal variables use a unique prefix (M_Var) to avoid warnings and namespace
% collisions with variables in the calling function's scope, ensuring macro hygiene.
-define(assertRequestResult(CLIENT, METHOD, PARAMS, EXPECTED_RESULT), fun() ->
    M_Id = send_request(CLIENT, METHOD, PARAMS), % Send request, get ID
    ?assertReceiveResult(CLIENT, M_Id, EXPECTED_RESULT) % Assert the result for this ID
end()).

% Asserts that the next received message is a JSON-RPC error with EXPECTED_CODE and EXPECTED_ID.
% 1. Waits up to 1000ms for a response matching the EXPECTED_ID.
% 2. Asserts the response is an error with code EXPECTED_CODE.
% 3. Fails on timeout, if a success response is received, or if the error code/ID is different.
% 4. Temporarily stores unrelated messages received during the wait.
% 5. Restores unrelated messages to the process mailbox upon successful assertion.
% 6. Returns the matched error object.
% Internal variables use a unique prefix (M_Var) to avoid warnings and namespace
% collisions with variables in the calling function's scope, ensuring macro hygiene.
-define(assertReceiveError(CLIENT, EXPECTED_ID, EXPECTED_CODE), fun() ->
    {M_GunPid, M_WsRef} = CLIENT,
    M_TimeoutAt = erlang:system_time(millisecond) + 1000,
    M_WaitJsonRpc = fun M_WaitFun(M_MsgStack) ->
        M_Timeout = M_TimeoutAt - erlang:system_time(millisecond),
        receive
            {gun_ws, M_GunPid, M_WsRef, {text, M_Data}} = M_Msg ->
                case json_decode(M_Data) of
                    #{jsonrpc := <<"2.0">>, id := EXPECTED_ID,
                      error := #{code := EXPECTED_CODE} = M_Result} ->
                        {M_Result, M_MsgStack};
                    #{jsonrpc := <<"2.0">>, id := EXPECTED_ID,
                      error := M_UnexpectedError} ->
                        dump_mailbox(),
                        ct:fail({unexpected_error_code, ??EXPECTED_CODE,
                                 M_UnexpectedError, ?FUNCTION_NAME, ?LINE});
                    #{jsonrpc := <<"2.0">>, id := EXPECTED_ID, result := M_Result} ->
                        dump_mailbox(),
                        ct:fail({unexpected_result, M_Result, ?FUNCTION_NAME, ?LINE});
                    #{jsonrpc := <<"2.0">>, id := EXPECTED_ID} = M_Response ->
                        dump_mailbox(),
                        ct:fail({unexpected_response, M_Response, ?FUNCTION_NAME, ?LINE});
                    _ ->
                        % Put back and keep waiting
                        M_WaitFun([M_Msg | M_MsgStack])
                end;
            M_OtherMsg ->
                % Catch non-gun_ws messages or messages from other connections
                % Put back and keep waiting
                M_WaitFun([M_OtherMsg | M_MsgStack])
        after M_Timeout ->
            dump_mailbox(),
            ct:fail({response_timeout, assertReceiveError, ??EXPECTED_ID, ?FUNCTION_NAME, ?LINE})
        end
    end,
    {M_Result, M_Msgs} = M_WaitJsonRpc([]),
    % Restore unprocessed messages
    lists:foreach(fun(M_Msg) -> self() ! M_Msg end, lists:reverse(M_Msgs)),
    M_Result
end()).

% Sends a request and asserts the response is a JSON-RPC error with EXPECTED_CODE.
% 1. Sends a JSON-RPC request using the CLIENT, METHOD, and PARAMS, obtaining a request ID.
% 2. Calls assertReceiveError to wait for and validate the error response for that ID.
% 3. Returns the matched error object.
% Internal variables use a unique prefix (M_Var) to avoid warnings and namespace
% collisions with variables in the calling function's scope, ensuring macro hygiene.
-define(assertRequestError(CLIENT, METHOD, PARAMS, EXPECTED_CODE), fun() ->
    M_Id = send_request(CLIENT, METHOD, PARAMS), % Send request, get ID
    ?assertReceiveError(CLIENT, M_Id, EXPECTED_CODE) % Assert the error for this ID
end()).

% Waits for and asserts a notification matching METHOD and EXPECTED_PARAMS is received.
% 1. Waits up to 1000ms for a JSON-RPC notification message matching METHOD.
% 2. Asserts the notification's params field matches the EXPECTED_PARAMS pattern.
% 3. Fails on timeout or if the received parameters do not match.
% 4. Temporarily stores unrelated messages received during the wait.
% 5. Restores unrelated messages to the process mailbox upon successful assertion.
% 6. Returns the matched parameters.
% Internal variables use a unique prefix (M_Var) to avoid warnings and namespace
% collisions with variables in the calling function's scope, ensuring macro hygiene.
-define(assertNotification(CLIENT, METHOD, EXPECTED_PARAMS), fun() ->
    {M_GunPid, M_WsRef} = CLIENT,
    M_TimeoutAt = erlang:system_time(millisecond) + 1000,
    M_WaitJsonRpc = fun M_F(M_MsgStack) ->
        M_Timeout = M_TimeoutAt - erlang:system_time(millisecond),
        receive
            {gun_ws, M_GunPid, M_WsRef, {text, M_ResponseData}} = M_Msg ->
                case json_decode(M_ResponseData) of
                    #{jsonrpc := <<"2.0">>, method := METHOD, params := EXPECTED_PARAMS = M_Params} ->
                        {M_Params, M_MsgStack};
                    #{jsonrpc := <<"2.0">>, method := METHOD, params := M_UnexpectedParams} ->
                        dump_mailbox(),
                        ct:fail({unexpected_notification, METHOD, ??EXPECTED_PARAMS,
                                 M_UnexpectedParams, ?FUNCTION_NAME, ?LINE});
                    _OtherJson -> % Catch other valid JSON messages not matching above
                        M_F([M_Msg | M_MsgStack]) % Put back and keep waiting
                end;
            M_OtherMsg -> % Catch non-gun_ws messages or messages from other connections
                M_F([M_OtherMsg | M_MsgStack]) % Put back and keep waiting
        after M_Timeout ->
            dump_mailbox(),
            ct:fail({notification_timeout, METHOD, ?FUNCTION_NAME, ?LINE})
        end
    end,
    {M_Result, M_Msgs} = M_WaitJsonRpc([]),
    lists:foreach(fun(M_Msg) -> self() ! M_Msg end, lists:reverse(M_Msgs)), % Restore unprocessed messages
    M_Result
end()).


%=== CT CALLBACKS ==============================================================

all() -> [
    auth_success_test,
    auth_failure_test,
    topic_list_test,
    topic_get_test,
    bridge_list_test,
    topic_set_bandwidth_test,
    topic_update_notification_test,
    bridge_notification_test,
    multiple_connections_test,
    connection_cleanup_test,
    invalid_request_test,
    invalid_method_test,
    invalid_params_test
].

init_per_suite(Config) ->
    % Start required applications
    {ok, _} = application:ensure_all_started(gun),
    {ok, _} = application:ensure_all_started(cowboy),
    % Start process group
    {ok, PgPid} = pg:start(?PG_SCOPE),
    [{pg_scope, PgPid} | Config].

end_per_suite(Config) ->
    PgPid = proplists:get_value(pg_scope, Config),
    erlang:exit(PgPid, shutdown),
    application:stop(gun),
    application:stop(cowboy),
    ok.

init_per_testcase(connection_cleanup_test, Config) ->
    % Set up ETS tables for test data
    ets:new(?TEST_BRIDGES_TABLE, [named_table, set, public]),
    ets:new(?TEST_TOPICS_TABLE, [named_table, set, public]),

    % Start WebSocket handler with short ping settings for faster timeout testing
    start_ws_handler(500, 1000),

    Config;
init_per_testcase(_TestCase, Config) ->
    % Set up ETS tables for test data
    ets:new(?TEST_BRIDGES_TABLE, [named_table, set, public]),
    ets:new(?TEST_TOPICS_TABLE, [named_table, set, public]),

    % Start WebSocket handler with default ping settings
    start_ws_handler(30000, 60000),

    Config.

end_per_testcase(_TestCase, Config) ->
    % Clean up ETS tables
    ets:delete(?TEST_TOPICS_TABLE),
    ets:delete(?TEST_BRIDGES_TABLE),

    % Stop Cowboy
    cowboy:stop_listener(test_http),

    Config.


%=== TEST CASES ================================================================

%% Authentication tests

auth_success_test(_Config) ->
    Client = start_ws_client(?TEST_TOKEN),
    try
        ?assertRequestResult(Client, <<"bridge.list">>, #{}, [])
    after
        close_ws_client(Client)
    end.

auth_failure_test(_Config) ->
    % 1. Invalid token
    ?assertError({unexpected_http_response, 401},
                 start_ws_client(<<"invalid_token">>)),
    % 2. Missing token
    ?assertError({unexpected_http_response, 400},
                 start_ws_client("localhost", ?TEST_PORT, <<"/ro2erl_hub/ws">>)),
    ok.

%% Basic API tests
topic_list_test(_Config) ->
    Topic1 = add_test_topic(#{
        topic_name => <<"topic1">>,
        filterable => true,
        bandwidth_limit => 1000,
        metrics => #{
            dispatched => #{bandwidth => 100, rate => 10.0},
            forwarded => #{bandwidth => 50, rate => 5.0}
        }
    }),
    Topic2 = add_test_topic(#{
        topic_name => <<"topic2">>,
        filterable => false,
        bandwidth_limit => 2000,
        metrics => #{
            dispatched => #{bandwidth => 200, rate => 20.0},
            forwarded => #{bandwidth => 150, rate => 15.0}
        }
    }),

    Client = start_ws_client(?TEST_TOKEN),
    try
        Topics = ?assertRequestResult(Client, <<"topic.list">>, #{}, [_, _]),
        ?assertEqual([Topic1, Topic2], lists:sort(Topics))
    after
        close_ws_client(Client)
    end.

topic_get_test(_Config) ->
    TestTopic = add_test_topic(#{
        topic_name => <<"test_topic">>,
        filterable => true,
        bandwidth_limit => 1000,
        metrics => #{
            dispatched => #{bandwidth => 100, rate => 10.0},
            forwarded => #{bandwidth => 50, rate => 5.0}
        }
    }),

    Client = start_ws_client(?TEST_TOKEN),
    try
        ?assertRequestResult(Client, <<"topic.get">>, #{
            topic_name => <<"test_topic">>
        }, TestTopic),
        ?assertRequestError(Client, <<"topic.get">>, #{
            topic_name => <<"nonexistent_topic">>
        }, ?JSONRPC_TOPIC_NOT_FOUND)
    after
        close_ws_client(Client)
    end.

bridge_list_test(_Config) ->
    Client = start_ws_client(?TEST_TOKEN),
    try
        ?assertRequestResult(Client, <<"bridge.list">>, #{}, []),

        % Test with three bridges
        Bridge1 = add_test_bridge(<<"bridge1">>),
        Bridge2 = add_test_bridge(<<"bridge2">>),
        Bridge3 = add_test_bridge(<<"bridge3">>),

        BridgeList = ?assertRequestResult(Client, <<"bridge.list">>, #{}, _),
        ?assertEqual([Bridge1, Bridge2, Bridge3], lists:sort(BridgeList))

    after
        close_ws_client(Client)
    end.

topic_set_bandwidth_test(_Config) ->
    % Add test topics to the ETS table
    FilterableTopic = add_test_topic(#{
        topic_name => <<"filterable_topic">>,
        filterable => true,
        bandwidth_limit => 1000,
        metrics => #{
            dispatched => #{bandwidth => 100, rate => 10.0},
            forwarded => #{bandwidth => 50, rate => 5.0}
        }
    }),
    NonFilterableTopic = add_test_topic(#{
        topic_name => <<"nonfilterable_topic">>,
        filterable => false,
        bandwidth_limit => null,
        metrics => #{
            dispatched => #{bandwidth => 200, rate => 20.0},
            forwarded => #{bandwidth => 150, rate => 15.0}
        }
    }),

    Client = start_ws_client(?TEST_TOKEN),
    try
        % Test getting topic info before changing bandwidth
        ?assertRequestResult(Client, <<"topic.get">>, #{
            topic_name => <<"filterable_topic">>
        }, FilterableTopic),
        ?assertRequestResult(Client, <<"topic.get">>, #{
            topic_name => <<"nonfilterable_topic">>
        }, NonFilterableTopic),

        % Test setting bandwidth on filterable topic
        ?assertRequestResult(Client, <<"topic.setBandwidth">>, #{
            topic_name => <<"filterable_topic">>,
            bandwidth => 2000
        }, <<"ok">>),

        % Verify bandwidth was updated
        UpdatedTopic = ?assertRequestResult(Client, <<"topic.get">>, #{
            topic_name => <<"filterable_topic">>
        }, _),
        ?assertEqual(2000, maps:get(bandwidth_limit, UpdatedTopic)),

        % Test removing bandwidth limit
        ?assertRequestResult(Client, <<"topic.setBandwidth">>, #{
            topic_name => <<"filterable_topic">>,
            bandwidth => null
        }, <<"ok">>),

        % Verify bandwidth limit was removed
        UnlimitedTopic = ?assertRequestResult(Client, <<"topic.get">>, #{
            topic_name => <<"filterable_topic">>
        }, _),
        ?assertEqual(null, maps:get(bandwidth_limit, UnlimitedTopic)),

        % Test setting bandwidth on non-filterable topic (should fail)
        ?assertRequestError(Client, <<"topic.setBandwidth">>, #{
            topic_name => <<"nonfilterable_topic">>,
            bandwidth => 1500
        }, ?JSONRPC_NOT_FILTERABLE),

        % Test setting bandwidth on non-existent topic
        ?assertRequestError(Client, <<"topic.setBandwidth">>, #{
            topic_name => <<"nonexistent_topic">>,
            bandwidth => 1000
        }, ?JSONRPC_TOPIC_NOT_FOUND)
    after
        close_ws_client(Client)
    end.

topic_update_notification_test(_Config) ->
    % Add initial test topic to the ETS table
    InitialTopic = add_test_topic(#{
        topic_name => <<"notification_topic">>,
        filterable => true,
        bandwidth_limit => 1000,
        metrics => #{
            dispatched => #{bandwidth => 100, rate => 10.0},
            forwarded => #{bandwidth => 50, rate => 5.0}
        }
    }),

    % Connect to the WebSocket server
    Client = start_ws_client(?TEST_TOKEN),
    try
        % Verify initial topic state
        ?assertRequestResult(Client, <<"topic.get">>, #{
            topic_name => <<"notification_topic">>
        }, InitialTopic),

        % Update topic bandwidth using the API
        ?assertRequestResult(Client, <<"topic.setBandwidth">>, #{
            topic_name => <<"notification_topic">>,
            bandwidth => 2000
        }, <<"ok">>),

        % Wait for and verify the notification is received
        ?assertNotification(Client, <<"topic.updated">>, #{
            topic_name := <<"notification_topic">>,
            bandwidth_limit := 2000
        }),

        % Verify topic was updated
        ?assertRequestResult(Client, <<"topic.get">>, #{
            topic_name => <<"notification_topic">>
        }, #{
            topic_name := <<"notification_topic">>,
            bandwidth_limit := 2000
        }),

        % Remove bandwidth limit and check notification
        ?assertRequestResult(Client, <<"topic.setBandwidth">>, #{
            topic_name => <<"notification_topic">>,
            bandwidth => null
        }, <<"ok">>),

        % Verify second notification
        ?assertNotification(Client, <<"topic.updated">>, #{
            topic_name := <<"notification_topic">>,
            bandwidth_limit := null
        }),

        % Add a new topic and check for notification
        add_test_topic(#{
            topic_name => <<"new_notification_topic">>,
            filterable => true,
            bandwidth_limit => 3000,
            metrics => #{
                dispatched => #{bandwidth => 300, rate => 30.0},
                forwarded => #{bandwidth => 250, rate => 25.0}
            }
        }),

        % Verify notification for new topic
        ?assertNotification(Client, <<"topic.updated">>, #{
            topic_name := <<"new_notification_topic">>,
            bandwidth_limit := 3000
        })
    after
        close_ws_client(Client)
    end.

bridge_notification_test(_Config) ->
    % Connect to the WebSocket server
    Client = start_ws_client(?TEST_TOKEN),
    try
        % Verify initial state - no bridges
        ?assertRequestResult(Client, <<"bridge.list">>, #{}, []),

        % Add a bridge and trigger a bridge.attached notification
        BridgeId1 = <<"test_bridge_1">>,
        BridgeInfo1 = add_test_bridge(BridgeId1),

        % Wait for and verify the notification is received
        ?assertNotification(Client, <<"bridge.attached">>, #{bridge_id := BridgeId1}),

        % Verify bridge list was updated
        ?assertRequestResult(Client, <<"bridge.list">>, #{}, [BridgeInfo1]),

        % Add a second bridge and trigger another notification
        BridgeId2 = <<"test_bridge_2">>,
        BridgeInfo2 = add_test_bridge(BridgeId2),

        % Verify the second notification
        ?assertNotification(Client, <<"bridge.attached">>, #{bridge_id := BridgeId2}),

        % Verify bridge list now contains both bridges
        Bridges = ?assertRequestResult(Client, <<"bridge.list">>, #{}, [_, _]),
        ?assertEqual([BridgeInfo1, BridgeInfo2], lists:sort(Bridges)),

        % Detach the first bridge and trigger a bridge.detached notification
        del_test_bridge(BridgeId1, <<"test_reason">>),

        % Verify the notification is received
        ?assertNotification(Client, <<"bridge.detached">>,
            #{bridge_id := BridgeId1, reason := <<"test_reason">>}),

        % Verify bridge list now contains only the second bridge
        ?assertRequestResult(Client, <<"bridge.list">>, #{}, [BridgeInfo2]),

        % Detach the second bridge and trigger another notification
        del_test_bridge(BridgeId2, <<"test_reason_2">>),

        % Verify the notification is received
        ?assertNotification(Client, <<"bridge.detached">>,
            #{bridge_id := BridgeId2, reason := <<"test_reason_2">>}),

        % Verify bridge list is empty
        ?assertRequestResult(Client, <<"bridge.list">>, #{}, [])

    after
        close_ws_client(Client),
        % Clean up bridge data
        ets:delete_all_objects(?TEST_BRIDGES_TABLE)
    end.

multiple_connections_test(_Config) ->
    % Connect with three separate WebSocket clients
    Client1 = start_ws_client(?TEST_TOKEN),
    Client2 = start_ws_client(?TEST_TOKEN),
    Client3 = start_ws_client(?TEST_TOKEN),

    try
        % Verify initial state for all clients - no topics, no bridges
        ?assertRequestResult(Client1, <<"topic.list">>, #{}, []),
        ?assertRequestResult(Client2, <<"topic.list">>, #{}, []),
        ?assertRequestResult(Client3, <<"topic.list">>, #{}, []),

        ?assertRequestResult(Client1, <<"bridge.list">>, #{}, []),
        ?assertRequestResult(Client2, <<"bridge.list">>, #{}, []),
        ?assertRequestResult(Client3, <<"bridge.list">>, #{}, []),

        % Add a test topic via API call on Client1
        TestTopic = add_test_topic(#{
            topic_name => <<"multi_conn_topic">>,
            filterable => true,
            bandwidth_limit => 1000,
            metrics => #{
                dispatched => #{bandwidth => 100, rate => 10.0},
                forwarded => #{bandwidth => 50, rate => 5.0}
            }
        }),

        % All clients should receive the topic.updated notification
        ?assertNotification(Client1, <<"topic.updated">>, #{topic_name := <<"multi_conn_topic">>}),
        ?assertNotification(Client2, <<"topic.updated">>, #{topic_name := <<"multi_conn_topic">>}),
        ?assertNotification(Client3, <<"topic.updated">>, #{topic_name := <<"multi_conn_topic">>}),

        % All clients should be able to get the topic info
        ?assertRequestResult(Client1, <<"topic.get">>, #{
            topic_name => <<"multi_conn_topic">>
        }, TestTopic),
        ?assertRequestResult(Client2, <<"topic.get">>, #{
            topic_name => <<"multi_conn_topic">>
        }, TestTopic),
        ?assertRequestResult(Client3, <<"topic.get">>, #{
            topic_name => <<"multi_conn_topic">>
        }, TestTopic),

        % Modify bandwidth using Client2
        ?assertRequestResult(Client2, <<"topic.setBandwidth">>, #{
            topic_name => <<"multi_conn_topic">>,
            bandwidth => 2000
        }, <<"ok">>),

        % All clients should receive the update notification
        ?assertNotification(Client1, <<"topic.updated">>, #{
            topic_name := <<"multi_conn_topic">>,
            bandwidth_limit := 2000
        }),
        ?assertNotification(Client2, <<"topic.updated">>, #{
            topic_name := <<"multi_conn_topic">>,
            bandwidth_limit := 2000
        }),
        ?assertNotification(Client3, <<"topic.updated">>, #{
            topic_name := <<"multi_conn_topic">>,
            bandwidth_limit := 2000
        }),

        % Add a bridge via API call
        BridgeId = <<"multi_conn_bridge">>,
        BridgeInfo = add_test_bridge(BridgeId),

        % All clients should receive the bridge.attached notification
        ?assertNotification(Client1, <<"bridge.attached">>, #{bridge_id := BridgeId}),
        ?assertNotification(Client2, <<"bridge.attached">>, #{bridge_id := BridgeId}),
        ?assertNotification(Client3, <<"bridge.attached">>, #{bridge_id := BridgeId}),

        % All clients should be able to get the bridge info
        ?assertRequestResult(Client1, <<"bridge.list">>, #{}, [BridgeInfo]),
        ?assertRequestResult(Client2, <<"bridge.list">>, #{}, [BridgeInfo]),
        ?assertRequestResult(Client3, <<"bridge.list">>, #{}, [BridgeInfo]),

        % Disconnect Client2 but keep the others connected
        close_ws_client(Client2),

        % Detach the bridge
        del_test_bridge(BridgeId, <<"test_reason">>),

        % Remaining clients should receive the notification
        ?assertNotification(Client1, <<"bridge.detached">>, #{
            bridge_id := BridgeId,
            reason := <<"test_reason">>
        }),
        ?assertNotification(Client3, <<"bridge.detached">>, #{
            bridge_id := BridgeId,
            reason := <<"test_reason">>
        }),

        % Remaining clients should see the empty bridge list
        ?assertRequestResult(Client1, <<"bridge.list">>, #{}, []),
        ?assertRequestResult(Client3, <<"bridge.list">>, #{}, [])

    after
        % Clean up all clients
        close_ws_client(Client1),
        % Client2 already closed in the test
        close_ws_client(Client3)
    end.

connection_cleanup_test(_Config) ->
    % Connect a client that will be properly closed
    GracefulClient = start_ws_client(?TEST_TOKEN),

    % Connect a client that will time out
    TimeoutClient = start_ws_client(?TEST_TOKEN),
    {TimeoutGunPid, _} = TimeoutClient,

    % Connect a client that will be abruptly disconnected
    AbruptClient = start_ws_client(?TEST_TOKEN),
    {AbruptGunPid, _} = AbruptClient,

    % Connect a monitoring client that will stay alive
    MonitorClient = start_ws_client(?TEST_TOKEN),

    % Add a test topic and bridge to verify notifications
    TestTopic = add_test_topic(<<"cleanup_test_topic">>),
    BridgeId = <<"cleanup_test_bridge">>,
    add_test_bridge(BridgeId),

    % Wait a bit to ensure setup is complete
    timer:sleep(100),

    % Get initial member count in the notification group
    InitialMembers = pg:get_local_members(?PG_SCOPE, ?PG_GROUP),
    InitialMemberCount = length(InitialMembers),
    ?assertEqual(4, InitialMemberCount), % All 4 clients should be in the group

    % Verify all clients are working
    ?assertRequestResult(GracefulClient, <<"topic.list">>, #{}, [TestTopic]),
    ?assertRequestResult(TimeoutClient, <<"topic.list">>, #{}, [TestTopic]),
    ?assertRequestResult(AbruptClient, <<"topic.list">>, #{}, [TestTopic]),
    ?assertRequestResult(MonitorClient, <<"topic.list">>, #{}, [TestTopic]),

    % Close GracefulClient properly
    close_ws_client(GracefulClient),

    % Simulate an abrupt disconnection for AbruptClient (kill the gun process)
    erlang:exit(AbruptGunPid, kill),

    % Wait a bit to ensure cleanup is complete
    timer:sleep(100),

    % Check that the notification group has been cleaned up
    RemainingMembers1 = pg:get_local_members(?PG_SCOPE, ?PG_GROUP),
    ?assertEqual(2, length(RemainingMembers1)),

    % Block the TimeoutClient from responding to pings (we stop the gun process)
    % and wait for timeout to occur (ping_interval + ping_timeout + buffer)
    sys:suspend(TimeoutGunPid, 2000),
    ?assertRequestResult(MonitorClient, <<"topic.list">>, #{}, [TestTopic]),
    wait(2500), % Wait for timeout to occur while handling ping frames

    % Check that the notification group has been cleaned up
    RemainingMembers2 = pg:get_local_members(?PG_SCOPE, ?PG_GROUP),
    ?assertEqual(1, length(RemainingMembers2)),

    % Verify MonitorClient is still working
    ?assertRequestResult(MonitorClient, <<"topic.list">>, #{}, [TestTopic]),

    % Check that notifications still work for the remaining client
    del_test_bridge(BridgeId, <<"test_reason">>),
    ?assertNotification(MonitorClient, <<"bridge.detached">>, #{
        bridge_id := BridgeId,
        reason := <<"test_reason">>
    }),

    % Clean up the last connection
    close_ws_client(MonitorClient),

    % Wait a bit to ensure cleanup completes
    timer:sleep(100),

    % Verify all group members are gone
    FinalMembers = pg:get_local_members(?PG_SCOPE, ?PG_GROUP),
    ?assertEqual(0, length(FinalMembers)),

    ok.

invalid_request_test(_Config) ->
    % Connect to the WebSocket server
    Client = start_ws_client(?TEST_TOKEN),

    try
        %% 1. Test Invalid JSON Syntax (-32700 Parse Error)
        InvalidJson = <<"{ invalid json }">>,
        send_frame(Client, {text, InvalidJson}), % Use send_frame helper
        {GunPid, WsRef} = Client, % Deconstruct Client before receive, use standard names
        receive
            % Use the locally bound GunPid and WsRef
            {gun_ws, GunPid, WsRef, {text, Data}} ->
                case json_decode(Data) of
                    #{jsonrpc := <<"2.0">>, id := null,
                      error := #{code := ?JSONRPC_PARSE_ERROR}} -> ok;
                    Response -> ct:fail({unexpected_response, invalid_json, Response})
                end
        after 1000 -> ct:fail({response_timeout, invalid_json})
        end,

        %% 2. Test Invalid JSON-RPC Request Structure (-32600 Invalid Request)

        % Case 2.1: Missing 'jsonrpc' field
        MissingJsonRpc = #{id => 1, method => <<"test">>},
        send_json(Client, MissingJsonRpc),
        % Id may or may not be valid, so we use _ as a wildcard
        ?assertReceiveError(Client, _, ?JSONRPC_INVALID_REQUEST),

        % Case 2.2: Missing 'method' field
        MissingMethod = #{jsonrpc => <<"2.0">>, id => 2},
        send_json(Client, MissingMethod),
        ?assertReceiveError(Client, 2, ?JSONRPC_INVALID_REQUEST),

        % Case 2.3: Invalid 'id' type (float)
        InvalidId = #{jsonrpc => <<"2.0">>, id => 3.14, method => <<"test">>},
        send_json(Client, InvalidId),
        % ID should be string, number or null
        ?assertReceiveError(Client, null, ?JSONRPC_INVALID_REQUEST),

        % Case 2.4: Invalid 'params' type (string instead of object/array for known method)
        InvalidParamsType = #{
            jsonrpc => <<"2.0">>,
            id => 4,
            method => <<"topic.get">>,
            params => <<"not_an_object">>
        },
        send_json(Client, InvalidParamsType),
        ?assertReceiveError(Client, 4, ?JSONRPC_INVALID_PARAMS),

        %% 3. Test Connection Stability
        % Send a valid request to ensure the connection is still functional
        ?assertRequestResult(Client, <<"bridge.list">>, #{}, [])
    after
        close_ws_client(Client)
    end.

invalid_method_test(_Config) ->
    % Connect to the WebSocket server
    Client = start_ws_client(?TEST_TOKEN),
    try
        % Send a request with a non-existent method
        ?assertRequestError(Client, <<"nonexistent.method">>, #{}, ?JSONRPC_METHOD_NOT_FOUND),

        % Send a valid request to ensure the connection is still functional
        ?assertRequestResult(Client, <<"bridge.list">>, #{}, [])
    after
        close_ws_client(Client)
    end.

invalid_params_test(_Config) ->
    % Connect to the WebSocket server with valid token
    Client = start_ws_client(?TEST_TOKEN),
    try

        % Missing required parameters for topic.get
        ?assertRequestError(Client, <<"topic.get">>, #{}, ?JSONRPC_INVALID_PARAMS),

        % Add a test topics for bandwidth tests
        add_test_topic(<<"test_invalid_params_topic">>),
        add_test_topic(#{
            topic_name => <<"test_invalid_params_topic_not_filterable">>,
            filterable => false
        }),

        % Negative bandwidth
        ?assertRequestError(Client, <<"topic.setBandwidth">>, #{
            topic_name => <<"test_invalid_params_topic">>,
            bandwidth => -10
        }, ?JSONRPC_INVALID_BANDWIDTH),

        % Zero bandwidth
        ?assertRequestError(Client, <<"topic.setBandwidth">>, #{
            topic_name => <<"test_invalid_params_topic">>,
            bandwidth => 0
        }, ?JSONRPC_INVALID_BANDWIDTH),

        % Non-existent topic
        ?assertRequestError(Client, <<"topic.setBandwidth">>, #{
            topic_name => <<"non_existent_topic">>,
            bandwidth => 1000
        }, ?JSONRPC_TOPIC_NOT_FOUND),

        % Topic not filterable
        ?assertRequestError(Client, <<"topic.setBandwidth">>, #{
            topic_name => <<"test_invalid_params_topic_not_filterable">>,
            bandwidth => 1000
            }, ?JSONRPC_NOT_FILTERABLE)

    after
        close_ws_client(Client)
    end.


%=== SERVER CALLBACKS ==========================================================

get_topics() ->
    % Return list of topics from ETS
    ets:foldl(fun({TopicName, TopicInfo}, Acc) ->
        Acc#{TopicName => TopicInfo}
    end, #{},?TEST_TOPICS_TABLE).

get_topic(TopicName) ->
    % Lookup topic in ETS
    case ets:lookup(?TEST_TOPICS_TABLE, TopicName) of
        [{TopicName, TopicInfo}] -> {ok, TopicInfo};
        [] -> {error, not_found}
    end.

get_bridges() ->
    % Return list of bridge IDs from ETS
    ets:foldl(fun({BridgeId, _}, Acc) ->
        [#{bridge_id => BridgeId} | Acc]
    end, [], ?TEST_BRIDGES_TABLE).

set_topic_bandwidth(TopicName, Bandwidth) ->
    % Set bandwidth limit for a topic in ETS
    case ets:lookup(?TEST_TOPICS_TABLE, TopicName) of
        [{TopicName, TopicInfo}] ->
            case maps:get(filterable, TopicInfo) of
                true ->
                    NewTopicInfo = TopicInfo#{bandwidth_limit => Bandwidth},
                    ets:insert(?TEST_TOPICS_TABLE, {TopicName, NewTopicInfo}),
                    notify_handler({topic_updated, NewTopicInfo}),
                    ok;
                false ->
                    {error, not_filterable}
            end;
        [] ->
            {error, not_found}
    end.


%=== INTERNAL HELPER FUNCTIONS =================================================

dump_mailbox() ->
    receive
        Msg ->
            ct:pal("Mailbox: ~p", [Msg]),
            dump_mailbox()
    after 0 ->
        ok
    end.

json_object_push(Key, Value, Acc) ->
    try
        AtomKey = binary_to_existing_atom(Key, utf8),
        [{AtomKey, Value} | Acc]
    catch
        error:badarg -> % Atom does not exist
            [{Key, Value} | Acc] % Keep as binary
    end.

json_decode(Data) ->
    {Result, _Acc, <<>>} =
        json:decode(Data, ok, #{object_push => fun json_object_push/3}),
    Result.

json_encode(Term) ->
    iolist_to_binary(json:encode(Term)).

start_ws_client(Token) ->
    Path = "/ro2erl_hub/ws?token=" ++ binary_to_list(Token),
    start_ws_client("localhost", ?TEST_PORT, Path).

start_ws_client(Host, Port, Path) ->
    % Connect to the WebSocket server with the given token
    {ok, GunPid} = gun:open(Host, Port, #{retry => 0}),
    {ok, _} = gun:await_up(GunPid),
    StreamRef = gun:ws_upgrade(GunPid, Path, [], #{silence_pings => false}),

    % Wait for successful connection or failure
    receive
        {gun_upgrade, GunPid, StreamRef, [<<"websocket">>], _Headers} ->
            {GunPid, StreamRef};
        {gun_response, GunPid, StreamRef, _, Status, _Headers} ->
            gun:close(GunPid),
            error({unexpected_http_response, Status});
        {gun_error, GunPid, StreamRef, Reason} ->
            gun:close(GunPid),
            error({gun_error, Reason})
    after 1000 ->
        gun:close(GunPid),
        error({connection_timeout})
    end.

close_ws_client({GunPid, _WsRef}) ->
    % Close the WebSocket connection
    gun:close(GunPid).

% Wait a given time in milliseconds while handling ping frames for all clients
wait(Delay) ->
    wait_until(erlang:system_time(millisecond) + Delay).

% Wait until a specific absolute time in milliseconds while handling ping frames for all clients
wait_until(Until) ->
    Timeout = Until - erlang:system_time(millisecond),
    receive
        {gun_ws, Pid, WsRef, ping} ->
            gun:ws_send(Pid, WsRef, pong),
            wait_until(Until)
    after
        Timeout -> ok
    end.

send_request({GunPid, WsRef}, Method, Params) ->
    % Generate a unique ID for this request
    Id = integer_to_binary(erlang:unique_integer([positive])),

    % Create the JSON-RPC request object
    Request = #{
        jsonrpc => <<"2.0">>,
        method => Method,
        params => Params,
        id => Id
    },

    % Encode the request as JSON and send it
    send_json({GunPid, WsRef}, Request),

    % Return the ID so the caller can match the response
    Id.

send_frame({GunPid, WsRef}, Frame) ->
    gun:ws_send(GunPid, WsRef, Frame),
    ok.

send_json(Client, Request) ->
    JsonRequest = json_encode(Request),
    send_frame(Client, {text, JsonRequest}).

notify_handler(Notification) ->
    % Send notification to all members of the process group
    lists:foreach(fun(Member) ->
        Member ! {hub_notification, Notification}
    end, pg:get_local_members(?PG_SCOPE, ?PG_GROUP)).

add_test_topic(TopicName) when is_binary(TopicName) ->
    add_test_topic(#{topic_name => TopicName});
add_test_topic(TopicSpec = #{topic_name := TopicName}) ->
    TopicInfo = #{
        topic_name => TopicName,
        filterable => maps:get(filterable, TopicSpec, true),
        bandwidth_limit => maps:get(bandwidth_limit, TopicSpec, null),
        metrics => maps:get(metrics, TopicSpec, #{
            dispatched => #{bandwidth => 100, rate => 10.0},
            forwarded => #{bandwidth => 50, rate => 5.0}
        })
    },
    ets:insert(?TEST_TOPICS_TABLE, {TopicName, TopicInfo}),
    notify_handler({topic_updated, TopicInfo}),
    TopicInfo.

add_test_bridge(BridgeId) when is_binary(BridgeId) ->
    add_test_bridge(#{bridge_id => BridgeId});
add_test_bridge(BridgeInfo = #{bridge_id := BridgeId}) ->
    ets:insert(?TEST_BRIDGES_TABLE, {BridgeId, BridgeInfo}),
    notify_handler({bridge_attached, BridgeId}),
    BridgeInfo.

del_test_bridge(BridgeId, Reason) ->
    ets:delete(?TEST_BRIDGES_TABLE, BridgeId),
    notify_handler({bridge_detached, BridgeId, Reason}),
    ok.

% Start WebSocket handler with configurable ping settings
start_ws_handler(PingInterval, PingTimeout) ->
    % Setup Cowboy with the WebSocket handler
    Dispatch = cowboy_router:compile([
        {'_', [{"/ro2erl_hub/ws", ro2erl_hub_ws_handler, #{
            server_mod => ?MODULE,
            ws_pg_info => {?PG_SCOPE, ?PG_GROUP},
            auth_token => ?TEST_TOKEN,
            ping_interval => PingInterval,
            ping_timeout => PingTimeout
        }}]}
    ]),
    {ok, _} = cowboy:start_clear(test_http, [{port, ?TEST_PORT}],
                                 #{env => #{dispatch => Dispatch}}),
    ok.
