-module(bf_bot_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

-define(APPS, [log4erl, bf_bot]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

%% to start manually from console with start.sh
start() ->
    [begin application:start(A), io:format("~p~n", [A]) end || A <- ?APPS].

start(_StartType, _StartArgs) ->
    Config = bf_bot_util:log4erl_config(),
    log4erl:conf(Config),
    log4erl:info("starting bf_bot"),
    bf_bot_sup:start_link().

stop(_State) ->
    ok.

stop() ->
    log4erl:info("stopping bf_gateway"),
    [application:stop(A) || A <- lists:reverse(?APPS)].

