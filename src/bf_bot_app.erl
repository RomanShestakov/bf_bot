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
    bf_bot_sup:start_link().

stop(_State) ->
    ok.

stop() ->
    log4erl:info("stopping bf_gateway"),
    [application:stop(A) || A <- lists:reverse(?APPS)].

