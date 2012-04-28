%% Copyright (C) 2011 Roman Shestakov
%%%
%%% This file is part of bf_bot
%%%
%%% bf_bot is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as 
%%% published by the Free Software Foundation, either version 3 of 
%%% the License, or (at your option) any later version.
%%%
%%% bf_bot is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public 
%%% License along with Erlsom.  If not, see 
%%% <http://www.gnu.org/licenses/>.
%%%
%%% Author contact: romanshestakov@yahoo.co.uk

-module(bf_bot_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).
-define(APPS, [log4erl, inets, bf_bot]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% to start manually from console with start.sh
start() ->
    [begin application:start(A), io:format("~p~n", [A]) end || A <- ?APPS].

start(_StartType, _StartArgs) ->
    case application:get_env(bf_bot, marketId) of
	{ok, MarketId} -> 
	    Config = bf_bot_util:log4erl_config(),
	    log4erl:conf(Config),
	    log4erl:info("starting bf_bot"),
	    bf_bot_sup:start_link(MarketId);
	undefined -> throw({error, marketId_not_defined})
    end.

stop(_State) ->
    ok.

stop() ->
    log4erl:info("stopping bf_bot"),
    [application:stop(A) || A <- lists:reverse(?APPS)].

