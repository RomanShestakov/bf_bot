%% Copyright (C) 2011 Roman Shestakov
%%%
%%% This file is part of bf_gateway
%%%
%%% bf_gateway is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as 
%%% published by the Free Software Foundation, either version 3 of 
%%% the License, or (at your option) any later version.
%%%
%%% bf_gateway is distributed in the hope that it will be useful,
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
 	 get_marketId/0
%% 	 get_password/0,
%% 	 get_GS_Wsdl/0,
%% 	 get_GX_Wsdl/0
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

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% get passord
%% %% @end
%% %%--------------------------------------------------------------------
%% -spec get_password() -> string() | no_return().
%% get_password() ->
%%     case application:get_env(bf_gateway, password) of
%% 	{ok, Value} -> Value;
%% 	undefined -> throw({error, password_not_defined})
%%     end.

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% get BFGlobalService.wsdl
%% %% @end
%% %%--------------------------------------------------------------------
%% -spec get_GS_Wsdl() -> string().
%% get_GS_Wsdl() ->
%%     "file://" ++ code:priv_dir(bf_gateway) ++ "/BFGlobalService.wsdl".

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% get BFExchangeService.wsdl
%% %% @end
%% %%--------------------------------------------------------------------
%% -spec get_GX_Wsdl() -> string().
%% get_GX_Wsdl() ->
%%     "file://" ++ code:priv_dir(bf_gateway) ++ "/BFExchangeService.wsdl".
