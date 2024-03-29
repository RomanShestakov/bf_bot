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

-module(bf_bot_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
%%-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(MarketId) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MarketId]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([MarketId]) ->
    Bfbot = {'bf_bot', {'bf_bot', start_link, [MarketId]}, permanent, 2000, worker, ['bf_bot']},
    {ok, {{one_for_one, 5, 10}, [Bfbot]}}.
