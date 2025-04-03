-module(ro2erl_hub_server_SUITE).

-moduledoc """
Test suite for ro2erl_hub_server

## Current Test Coverage
- Basic bridge management (attach/detach)
- Message dispatching
- Multiple bridges interaction
- Message filtering (bridges don't receive their own messages)
- Bridge crash handling

## Future Test Cases
The following test cases are planned for future implementation:

### Error Handling
- Invalid message handling
- Invalid bridge ID handling

### Performance
- Large number of bridges (100+)
- Message throughput under load
- Memory usage monitoring

### Edge Cases
- Rapid attach/detach cycles
- Very large message handling
- Concurrent message sending
- State transition edge cases

### System Integration
- Integration with actual bridge implementations
- Network partition scenarios
- Node failure scenarios
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
    % Basic tests
    attach_test/1,
    detach_test/1,
    dispatch_test/1,
    % Advanced tests
    multiple_bridges_test/1,
    bridge_crash_test/1,
    % Topic and notification tests
    topic_management_test/1,
    ws_notification_test/1,
    set_topic_bandwidth_test/1
]).

%% Bridge API - Used by the hub
-export([
    dispatch/3,
    set_topic_bandwidth/3
]).


%=== MACROS ===================================================================

%% Test constants
-define(BRIDGE_ID(N), list_to_binary(io_lib:format("bridge_~b", [N]))).

%% Assertion macros
-define(assertAttached(ID), fun() ->
    M_AttachedBridges = ro2erl_hub_server:get_bridges(),
    ?assert(lists:member(ID, M_AttachedBridges),
            {bridge_not_attached, ID, M_AttachedBridges})
end()).

-define(assertNotAttached(ID), fun() ->
    M_AttachedBridges = ro2erl_hub_server:get_bridges(),
    ?assertNot(lists:member(ID, M_AttachedBridges),
               {bridge_still_attached, ID, M_AttachedBridges})
end()).

-define(assertDispatched(BRIDGE_PID, MESSAGE), fun() ->
    receive
        {hub_dispatch, BRIDGE_PID, _Timestamp, M_Msg} when M_Msg == MESSAGE -> ok
    after 1000 ->
        ct:fail({dispatch_timeout, MESSAGE, ?MODULE, ?LINE})
    end
end()).

-define(assertNoMessage(), fun() ->
    receive
        M_Any -> ct:fail({unexpected_message, M_Any, ?MODULE, ?LINE})
    after 300 ->
        ok
    end
end()).

%% Notification assertion macros
-define(assertNotifyAttached(BRIDGE_ID), fun() ->
    receive
        {hub_notification, {bridge_attached, BRIDGE_ID}} -> ok
    after 1000 ->
        ct:fail({notification_timeout, bridge_attached, ??BRIDGE_ID, ?MODULE, ?LINE})
    end
end()).

-define(assertNotifyDetached(BRIDGE_ID), fun() ->
    receive
        {hub_notification, {bridge_detached, BRIDGE_ID}} -> ok;
        {hub_notification, {bridge_detached, BRIDGE_ID, _Reason}} -> ok
    after 1000 ->
        ct:fail({notification_timeout, bridge_detached, ??BRIDGE_ID, ?MODULE, ?LINE})
    end
end()).

%% Simple topic update notification check (just verifies topic name)
-define(assertNotifyTopicUpdated(TOPIC_NAME), fun() ->
    receive
        {hub_notification, {topic_updated, #{topic_name := TOPIC_NAME}}} ->
            ok
    after 1000 ->
        ct:fail({notification_timeout, topic_updated, ??TOPIC_NAME, ?MODULE, ?LINE})
    end
end()).

%% Topic update notification check with validation function
-define(assertNotifyTopicUpdated(TOPIC_NAME, FUN), fun() ->
    receive
        {hub_notification, {topic_updated, M_TopicInfo = #{topic_name := TOPIC_NAME}}} ->
            FUN(M_TopicInfo)
    after 1000 ->
        ct:fail({notification_timeout, topic_updated, ??TOPIC_NAME, ?MODULE, ?LINE})
    end
end()).

-define(assertNoNotification(), fun() ->
    receive
        {hub_notification, M_Notification} ->
            ct:fail({unexpected_notification, M_Notification, ?MODULE, ?LINE})
    after 300 ->
        ok
    end
end()).

-define(assertTopicBandwidthSet(BRIDGE_PID, TOPIC_NAME, BANDWIDTH), fun() ->
    receive
        {hub_set_topic_bandwidth, BRIDGE_PID, TOPIC_NAME, M_Bandwidth} when M_Bandwidth =:= BANDWIDTH ->
            ok
    after 1000 ->
        ct:fail({bandwidth_set_timeout, TOPIC_NAME, BANDWIDTH, ?MODULE, ?LINE})
    end
end()).


%=== CT CALLBACKS =============================================================

all() -> [
    attach_test,
    detach_test,
    dispatch_test,
    multiple_bridges_test,
    bridge_crash_test,
    topic_management_test,
    ws_notification_test,
    set_topic_bandwidth_test
].

init_per_suite(Config) ->
    % Start net_kernel if not already started
    case net_kernel:start([ct_master, shortnames]) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        Error -> ct:fail({failed_to_start_net_kernel, Error})
    end,
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    % Start process group scope
    {ok, _} = pg:start_link(test_scope),

    % Start the hub server
    {ok, HubPid} = ro2erl_hub_server:start_link(
        {test_scope, test_group},
        {test_scope, ws_notification_group},
        ?MODULE
    ),

    [{hub_pid, HubPid} | Config].

end_per_testcase(_TestCase, Config) ->
    % Stop the hub server
    HubPid = proplists:get_value(hub_pid, Config),
    gen_statem:stop(HubPid),
    ok.


%=== TEST CASES ==============================================================

%% Basic Tests
attach_test(Config) ->
    % Register test process
    register(current_test, self()),

    % Create a simulated bridge
    BridgeId = ?BRIDGE_ID(1),
    BridgePid = start_bridge(self()),

    % Attach the bridge to the hub
    HubPid = proplists:get_value(hub_pid, Config),
    ok = bridge_attach(HubPid, BridgeId, BridgePid),

    % Verify attachment
    ?assertAttached(BridgeId),

    % Try attaching again - should not fail
    ok = bridge_attach(HubPid, BridgeId, BridgePid),
    ?assertAttached(BridgeId),

    % Cleanup
    stop_bridge(BridgePid),

    ?assertNoMessage(),
    ok.

detach_test(Config) ->
    % Register test process
    register(current_test, self()),

    % Create a simulated bridge
    BridgeId = ?BRIDGE_ID(1),
    BridgePid = start_bridge(self()),

    % Attach the bridge to the hub
    HubPid = proplists:get_value(hub_pid, Config),
    ok = bridge_attach(HubPid, BridgeId, BridgePid),
    ?assertAttached(BridgeId),

    % Detach the bridge
    ok = bridge_detach(HubPid, BridgePid),
    ?assertNotAttached(BridgeId),

    % Try detaching again - should still work
    ok = bridge_detach(HubPid, BridgePid),
    ?assertNotAttached(BridgeId),

    % Cleanup
    stop_bridge(BridgePid),

    ?assertNoMessage(),
    ok.

dispatch_test(Config) ->
    % Register test process
    register(current_test, self()),

    % Create a simulated bridge
    BridgeId = ?BRIDGE_ID(1),
    BridgePid = start_bridge(self()),

    % Attach the bridge to the hub
    HubPid = proplists:get_value(hub_pid, Config),
    ok = bridge_attach(HubPid, BridgeId, BridgePid),
    ?assertAttached(BridgeId),

    % Send a message through the hub (testing direct API)
    TestMessage = {test_message, <<"Test message">>},
    ok = ro2erl_hub_server:dispatch(TestMessage),

    % Verify the message was forwarded to the bridge
    ?assertDispatched(BridgePid, TestMessage),

    % Send a message on behalf of the bridge
    BridgeMessage = {test_message, <<"Message from bridge">>},
    ok = bridge_dispatch(HubPid, BridgePid, BridgeMessage),

    % Verify no message is received (bridge shouldn't receive its own message)
    ?assertNoMessage(),

    % Cleanup
    stop_bridge(BridgePid),

    ?assertNoMessage(),
    ok.

%% Advanced Tests
multiple_bridges_test(Config) ->
    % Register test process
    register(current_test, self()),

    % Create multiple simulated bridges
    Bridge1Id = ?BRIDGE_ID(1),
    Bridge2Id = ?BRIDGE_ID(2),
    Bridge1Pid = start_bridge(self()),
    Bridge2Pid = start_bridge(self()),

    % Attach both bridges to the hub
    HubPid = proplists:get_value(hub_pid, Config),
    ok = bridge_attach(HubPid, Bridge1Id, Bridge1Pid),
    ok = bridge_attach(HubPid, Bridge2Id, Bridge2Pid),
    ?assertAttached(Bridge1Id),
    ?assertAttached(Bridge2Id),

    % Send a message from Bridge1
    TestMessage = {test_message, <<"Test from Bridge1">>},
    ok = bridge_dispatch(HubPid, Bridge1Pid, TestMessage),

    % Verify Bridge2 received the message (Bridge1 shouldn't receive its own message)
    ?assertDispatched(Bridge2Pid, TestMessage),
    ?assertNoMessage(),

    % Send a message from Bridge2
    TestMessage2 = {test_message, <<"Test from Bridge2">>},
    ok = bridge_dispatch(HubPid, Bridge2Pid, TestMessage2),

    % Verify Bridge1 received the message (Bridge2 shouldn't receive its own message)
    ?assertDispatched(Bridge1Pid, TestMessage2),
    ?assertNoMessage(),

    % Test direct API dispatch with multiple bridges
    TestMessage3 = {test_message, <<"Test from direct API">>},
    ok = ro2erl_hub_server:dispatch(TestMessage3),

    % Verify both bridges received the message
    ?assertDispatched(Bridge1Pid, TestMessage3),
    ?assertDispatched(Bridge2Pid, TestMessage3),
    ?assertNoMessage(),

    % Detach Bridge1
    ok = bridge_detach(HubPid, Bridge1Pid),
    ?assertNotAttached(Bridge1Id),
    ?assertAttached(Bridge2Id),

    % Send another message - only Bridge2 should receive it
    TestMessage4 = {test_message, <<"Test after Bridge1 detached">>},
    ok = ro2erl_hub_server:dispatch(TestMessage4),
    ?assertDispatched(Bridge2Pid, TestMessage4),
    ?assertNoMessage(),

    % Cleanup
    stop_bridge(Bridge1Pid),
    stop_bridge(Bridge2Pid),

    ?assertNoMessage(),
    ok.

bridge_crash_test(Config) ->
    % Register test process
    register(current_test, self()),

    % Create multiple simulated bridges
    Bridge1Id = ?BRIDGE_ID(1),
    Bridge2Id = ?BRIDGE_ID(2),
    Bridge3Id = ?BRIDGE_ID(3),
    Bridge1Pid = start_bridge(self()),
    Bridge2Pid = start_bridge(self()),
    Bridge3Pid = start_bridge(self()),

    % Attach all bridges to the hub
    HubPid = proplists:get_value(hub_pid, Config),
    ok = bridge_attach(HubPid, Bridge1Id, Bridge1Pid),
    ok = bridge_attach(HubPid, Bridge2Id, Bridge2Pid),
    ok = bridge_attach(HubPid, Bridge3Id, Bridge3Pid),
    ?assertAttached(Bridge1Id),
    ?assertAttached(Bridge2Id),
    ?assertAttached(Bridge3Id),

    % Send a message to verify all bridges are working
    TestMessage = {test_message, <<"Test before crash">>},
    ok = ro2erl_hub_server:dispatch(TestMessage),
    ?assertDispatched(Bridge1Pid, TestMessage),
    ?assertDispatched(Bridge2Pid, TestMessage),
    ?assertDispatched(Bridge3Pid, TestMessage),
    ?assertNoMessage(),

    % Test message handling between bridges before crash
    TestMessage2 = {test_message, <<"Test from Bridge1">>},
    ok = bridge_dispatch(HubPid, Bridge1Pid, TestMessage2),
    ?assertDispatched(Bridge2Pid, TestMessage2),
    ?assertDispatched(Bridge3Pid, TestMessage2),
    ?assertNoMessage(),

    % Simulate Bridge1 crash
    Bridge1Pid ! crash,

    % Wait a bit to ensure the hub has time to detect the crash
    timer:sleep(100),

    % Verify Bridge1 is detached but others are still attached
    ?assertNotAttached(Bridge1Id),
    ?assertAttached(Bridge2Id),
    ?assertAttached(Bridge3Id),

    % Test message handling between remaining bridges
    TestMessage3 = {test_message, <<"Test from Bridge2">>},
    ok = bridge_dispatch(HubPid, Bridge2Pid, TestMessage3),
    ?assertDispatched(Bridge3Pid, TestMessage3),
    ?assertNoMessage(),

    TestMessage4 = {test_message, <<"Test from Bridge3">>},
    ok = bridge_dispatch(HubPid, Bridge3Pid, TestMessage4),
    ?assertDispatched(Bridge2Pid, TestMessage4),
    ?assertNoMessage(),

    % Test direct API dispatch to remaining bridges
    TestMessage5 = {test_message, <<"Test from direct API">>},
    ok = ro2erl_hub_server:dispatch(TestMessage5),
    ?assertDispatched(Bridge2Pid, TestMessage5),
    ?assertDispatched(Bridge3Pid, TestMessage5),
    ?assertNoMessage(),

    % Cleanup
    stop_bridge(Bridge2Pid),
    stop_bridge(Bridge3Pid),

    ok.

%% Topic and Notification Tests
topic_management_test(Config) ->
    % Register test process
    register(current_test, self()),

    % Create a simulated bridge
    BridgeId = ?BRIDGE_ID(1),
    BridgePid = start_bridge(self()),

    % Attach the bridge to the hub
    HubPid = proplists:get_value(hub_pid, Config),
    ok = bridge_attach(HubPid, BridgeId, BridgePid),
    ?assertAttached(BridgeId),

    % Initially, there should be no topics
    ?assertEqual(#{}, ro2erl_hub_server:get_topics()),

    % Update topics for the bridge
    TopicName1 = <<"topic1">>,
    TopicName2 = <<"topic2">>,
    Topics = #{
        TopicName1 => #{
            filterable => true,
            bandwidth_limit => 1000,
            metrics => #{
                dispatched => #{bandwidth => 100, rate => 10.0},
                forwarded => #{bandwidth => 50, rate => 5.0}
            }
        },
        TopicName2 => #{
            filterable => false,
            bandwidth_limit => 500,
            metrics => #{
                dispatched => #{bandwidth => 200, rate => 20.0},
                forwarded => #{bandwidth => 150, rate => 15.0}
            }
        }
    },
    bridge_update_topics(HubPid, BridgePid, Topics),

    % Allow time for topic update to be processed
    timer:sleep(100),

    % Verify topics are now available via get_topics()
    AllTopics = ro2erl_hub_server:get_topics(),
    ?assertEqual(2, maps:size(AllTopics)),
    ?assert(maps:is_key(TopicName1, AllTopics)),
    ?assert(maps:is_key(TopicName2, AllTopics)),

    % Verify individual topic retrieval works
    {ok, Topic1Info} = ro2erl_hub_server:get_topic(TopicName1),
    ?assertEqual(true, maps:get(filterable, Topic1Info)),
    ?assertEqual(1000, maps:get(bandwidth_limit, Topic1Info)),

    {ok, Topic2Info} = ro2erl_hub_server:get_topic(TopicName2),
    ?assertEqual(false, maps:get(filterable, Topic2Info)),
    ?assertEqual(500, maps:get(bandwidth_limit, Topic2Info)),

    % Test topic that doesn't exist
    ?assertEqual({error, not_found}, ro2erl_hub_server:get_topic(<<"nonexistent">>)),

    % Cleanup
    stop_bridge(BridgePid),

    ?assertNoMessage(),
    ok.

ws_notification_test(Config) ->
    % Register test process
    register(current_test, self()),

    % Join the notification process group to receive notifications
    ok = pg:join(test_scope, ws_notification_group, self()),

    % Verify we're in the process group
    Members = pg:get_local_members(test_scope, ws_notification_group),
    ?assert(lists:member(self(), Members)),

    % Create multiple simulated bridges
    Bridge1Id = ?BRIDGE_ID(1),
    Bridge2Id = ?BRIDGE_ID(2),
    Bridge1Pid = start_bridge(self()),
    Bridge2Pid = start_bridge(self()),

    % Attach both bridges to the hub
    HubPid = proplists:get_value(hub_pid, Config),
    ok = bridge_attach(HubPid, Bridge1Id, Bridge1Pid),
    ok = bridge_attach(HubPid, Bridge2Id, Bridge2Pid),

    % Should receive bridge_attached notifications for both bridges
    ?assertNotifyAttached(Bridge1Id),
    ?assertNotifyAttached(Bridge2Id),

    % Define topics for testing
    CommonTopic = <<"common_topic">>,
    Bridge1OnlyTopic = <<"bridge1_only">>,
    Bridge2OnlyTopic = <<"bridge2_only">>,

    % Set initial topics for Bridge1
    Bridge1Topics = #{
        CommonTopic => #{
            filterable => true,
            bandwidth_limit => 1000,
            metrics => #{
                dispatched => #{bandwidth => 100, rate => 10.0},
                forwarded => #{bandwidth => 50, rate => 5.0}
            }
        },
        Bridge1OnlyTopic => #{
            filterable => true,
            bandwidth_limit => 500,
            metrics => #{
                dispatched => #{bandwidth => 200, rate => 20.0},
                forwarded => #{bandwidth => 150, rate => 15.0}
            }
        }
    },
    bridge_update_topics(HubPid, Bridge1Pid, Bridge1Topics),

    % Should receive topic_update notifications for both topics from Bridge1
    ?assertNotifyTopicUpdated(CommonTopic),
    ?assertNotifyTopicUpdated(Bridge1OnlyTopic),
    ?assertNoNotification(),

    % Set initial topics for Bridge2 with different metrics for the common topic
    Bridge2Topics = #{
        CommonTopic => #{
            filterable => true,
            bandwidth_limit => 2000,
            metrics => #{
                dispatched => #{bandwidth => 300, rate => 30.0},
                forwarded => #{bandwidth => 250, rate => 25.0}
            }
        },
        Bridge2OnlyTopic => #{
            filterable => false,
            bandwidth_limit => 1500,
            metrics => #{
                dispatched => #{bandwidth => 400, rate => 40.0},
                forwarded => #{bandwidth => 350, rate => 35.0}
            }
        }
    },
    bridge_update_topics(HubPid, Bridge2Pid, Bridge2Topics),

    % Should receive topic_update notifications for both topics from Bridge2
    ?assertNotifyTopicUpdated(CommonTopic),
    ?assertNotifyTopicUpdated(Bridge2OnlyTopic),
    ?assertNoNotification(),

    % Now update only one topic in Bridge1
    UpdatedBridge1Topics = #{
        CommonTopic => #{
            filterable => true,
            bandwidth_limit => 800,  % Changed value
            metrics => #{
                dispatched => #{bandwidth => 150, rate => 15.0},  % Updated metrics
                forwarded => #{bandwidth => 75, rate => 7.5}      % Updated metrics
            }
        }
    },
    bridge_update_topics(HubPid, Bridge1Pid, UpdatedBridge1Topics),

    % Should receive notification only for the CommonTopic (not for Bridge1OnlyTopic)
    ?assertNotifyTopicUpdated(CommonTopic, fun(UpdatedTopicInfo) ->
        #{filterable := Filterable, bandwidth_limit := BandwidthLimit,
          metrics := Metrics} = UpdatedTopicInfo,
        % Should still be consolidated with Bridge2's data
        ?assertEqual(true, Filterable),
        % Should use the minimum bandwidth limit
        ?assertEqual(800, BandwidthLimit),
        % Metrics should be properly consolidated
        ?assertEqual(#{bandwidth => 450, rate => 45.0},
                     maps:get(dispatched, Metrics)),
        ?assertEqual(#{bandwidth => 325, rate => 32.5}, % 75+250, 7.5+25.0
                       maps:get(forwarded, Metrics))
    end),

    % Verify no additional notifications were received
    ?assertNoNotification(),

    % Test detach notifications
    ok = bridge_detach(HubPid, Bridge1Pid),
    ok = bridge_detach(HubPid, Bridge2Pid),

    % Should receive bridge_detached notifications for both bridges
    ?assertNotifyDetached(Bridge1Id),
    ?assertNotifyDetached(Bridge2Id),

    % Cleanup
    stop_bridge(Bridge1Pid),
    stop_bridge(Bridge2Pid),
    ok = pg:leave(test_scope, ws_notification_group, self()),

    ok.

set_topic_bandwidth_test(Config) ->
    % Register test process
    register(current_test, self()),
    % Join notification group
    ok = pg:join(test_scope, ws_notification_group, self()),

    HubPid = proplists:get_value(hub_pid, Config),
    BridgeId = ?BRIDGE_ID(1),
    BridgePid = start_bridge(self()),

    try
        % Attach bridge
        ok = bridge_attach(HubPid, BridgeId, BridgePid),
        ?assertNotifyAttached(BridgeId),

        % Define topics
        FilterableTopic = <<"filter_topic">>,
        NonFilterableTopic = <<"nonfilter_topic">>,

        % Bridge reports topics
        InitialTopics = #{ % Initial state
            FilterableTopic => #{
                filterable => true,
                bandwidth_limit => 1000,
                metrics => #{
                    dispatched => #{bandwidth => 10, rate => 1.0},
                    forwarded => #{bandwidth => 5, rate => 0.5}
                }
            },
            NonFilterableTopic => #{
                filterable => false,
                bandwidth_limit => infinity,
                metrics => #{
                    dispatched => #{bandwidth => 20, rate => 2.0},
                    forwarded => #{bandwidth => 15, rate => 1.5}
                }
            }
        },
        bridge_update_topics(HubPid, BridgePid, InitialTopics),
        ?assertNotifyTopicUpdated(FilterableTopic),
        ?assertNotifyTopicUpdated(NonFilterableTopic),
        ?assertNoNotification(),

        % 1. Test setting bandwidth on filterable topic
        NewBandwidth = 500,
        ?assertEqual(ok, ro2erl_hub_server:set_topic_bandwidth(FilterableTopic, NewBandwidth)),
        % Verify the mock bridge API was called
        ?assertTopicBandwidthSet(BridgePid, FilterableTopic, NewBandwidth),
        % Simulate bridge sending topic update with new bandwidth
        UpdatedTopics1 = InitialTopics#{
            FilterableTopic := #{
                filterable => true,
                bandwidth_limit => NewBandwidth,
                metrics => #{
                    dispatched => #{bandwidth => 10, rate => 1.0},
                    forwarded => #{bandwidth => 5, rate => 0.5}
                }
            }
        },
        bridge_update_topics(HubPid, BridgePid, UpdatedTopics1),
        % Verify notification was sent with new bandwidth
        ?assertNotifyTopicUpdated(FilterableTopic, fun(TopicInfo) ->
            ?assertEqual(NewBandwidth, maps:get(bandwidth_limit, TopicInfo))
        end),
        % Also get notification for the non-filterable topic (since we updated all topics)
        ?assertNotifyTopicUpdated(NonFilterableTopic),
        ?assertNoNotification(),

        % 2. Test removing bandwidth limit (setting to infinity)
        ?assertEqual(ok, ro2erl_hub_server:set_topic_bandwidth(FilterableTopic, infinity)),
        % Verify bridge API call
        ?assertTopicBandwidthSet(BridgePid, FilterableTopic, infinity),
        % Simulate bridge sending topic update with infinity bandwidth
        UpdatedTopics2 = UpdatedTopics1#{
            FilterableTopic := #{
                filterable => true,
                bandwidth_limit => infinity,
                metrics => #{
                    dispatched => #{bandwidth => 10, rate => 1.0},
                    forwarded => #{bandwidth => 5, rate => 0.5}
                }
            }
        },
        bridge_update_topics(HubPid, BridgePid, UpdatedTopics2),
        % Verify notification
        ?assertNotifyTopicUpdated(FilterableTopic, fun(TopicInfo) ->
            ?assertEqual(infinity, maps:get(bandwidth_limit, TopicInfo))
        end),
        % Also get notification for the non-filterable topic (since we updated all topics)
        ?assertNotifyTopicUpdated(NonFilterableTopic),
        ?assertNoNotification(),

        % 3. Test setting bandwidth on non-existent topic
        ?assertEqual({error, not_found}, ro2erl_hub_server:set_topic_bandwidth(<<"nonexistent">>, 1000)),
        ?assertNoMessage(), % No message to bridge
        ?assertNoNotification(),

        % 4. Test setting bandwidth on non-filterable topic
        ?assertEqual({error, not_filterable}, ro2erl_hub_server:set_topic_bandwidth(NonFilterableTopic, 1000)),
        ?assertNoMessage(), % No message to bridge
        ?assertNoNotification()

    after
        % Cleanup
        ok = pg:leave(test_scope, ws_notification_group, self()),
        stop_bridge(BridgePid)
    end.


%=== BRIDGE API IMPLEMENTATION ===================================================

%% Bridge API Implementation - These are called by the hub
dispatch(BridgePid, Timestamp, Message) ->
    current_test ! {hub_dispatch, BridgePid, Timestamp, Message},
    ok.

set_topic_bandwidth(BridgePid, TopicName, Bandwidth) ->
    % Simulate the bridge receiving the command by sending a message to the test process
    current_test ! {hub_set_topic_bandwidth, BridgePid, TopicName, Bandwidth},
    ok.

%=== INTERNAL HELPER FUNCTIONS =================================================

start_bridge(TestPid) ->
    spawn(fun() -> bridge_proc(TestPid) end).

stop_bridge(BridgePid) ->
    MonRef = erlang:monitor(process, BridgePid),
    BridgePid ! stop,
    receive
        {'DOWN', MonRef, process, BridgePid, _} -> ok
    after 1000 ->
        ct:fail(stop_timeout)
    end.

bridge_attach(HubPid, BridgeId, BridgePid) ->
    gen_statem:cast(HubPid, {bridge_attach, BridgeId, BridgePid}).

bridge_detach(HubPid, BridgePid) ->
    gen_statem:cast(HubPid, {bridge_detach, BridgePid}).

bridge_update_topics(HubPid, BridgePid, Topics) ->
    gen_statem:cast(HubPid, {bridge_update_topics, BridgePid, Topics}).

bridge_dispatch(HubPid, BridgePid, Message) ->
    Timestamp = erlang:system_time(millisecond),
    gen_statem:cast(HubPid, {bridge_dispatch, BridgePid, Timestamp, Message}).

bridge_proc(TestPid) ->
    receive
        {hub_dispatch, _BridgePid, _Timestamp, _Message} = Msg ->
            TestPid ! Msg,
            bridge_proc(TestPid);
        crash ->
            exit(crash);
        stop ->
            ok
    end.
