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

-module(bf_bot_util).

-export([
	 log4erl_config/0,
 	 get_marketId/0,
	 get_gateway_host/0,
	 get_gateway_port/0,
	 get_tickkeeper_node/0
	]).

%%--------------------------------------------------------------------
%% @doc
%% Get log config of the project
%% creates a full path name to the job file definition.
%% @end
%%--------------------------------------------------------------------
-spec log4erl_config() -> string() | no_return().
log4erl_config() ->
    case application:get_env(bf_bot, log4erl_config) of
	{ok, Value} -> Value;
	undefined -> throw({error, log4erl_config_not_defined})
    end.

%%--------------------------------------------------------------------
%% @doc
%% get betfair username
%% @end
%%--------------------------------------------------------------------
-spec get_marketId() -> integer() | no_return().
get_marketId() ->
    case application:get_env(bf_bot, marketId) of
	{ok, Value} -> Value;
	undefined -> throw({error, marketId_not_defined})
    end.
%%--------------------------------------------------------------------
%% @doc
%% get hostname or ipAddress of bf_gateway
%% @end
%%--------------------------------------------------------------------
-spec get_gateway_host() -> string() | no_return().
get_gateway_host() ->
    case application:get_env(bf_bot, gateway_host) of
	{ok, Value} -> Value;
	undefined -> throw({error, gateway_host_not_defined})
    end.

%%--------------------------------------------------------------------
%% @doc
%% get port of bf_gateway
%% @end
%%--------------------------------------------------------------------
-spec get_gateway_port() -> integer() | no_return().
get_gateway_port() ->
    case application:get_env(bf_bot, gateway_port) of
	{ok, Value} -> Value;
	undefined -> throw({error, gateway_port_not_defined})
    end.

%%--------------------------------------------------------------------
%% @doc
%% get node of tickkeeper
%% @end
%%--------------------------------------------------------------------
-spec get_tickkeeper_node() -> string() | no_return().
get_tickkeeper_node() ->
    case application:get_env(bf_bot, tickkeeper_node) of
	{ok, Value} -> Value;
	undefined -> throw({error, tickkeeper_node_not_defined})
    end.

