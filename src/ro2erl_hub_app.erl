%% -*- mode: erlang -*-

-module(ro2erl_hub_app).

-moduledoc """
Application callback module for ro2erl_hub.

This module implements the application behaviour and is responsible for starting
the supervision tree.
""".

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%=== BEHAVIOUR application CALLBACK FUNCTIONS ==================================

start(_StartType, _StartArgs) ->
    ro2erl_hub_sup:start_link().

stop(_State) ->
    ok.
