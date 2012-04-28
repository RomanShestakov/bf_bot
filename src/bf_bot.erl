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


-module(bf_bot).

-behaviour(gen_server).

%% API
-export([start_link/1,
	 subscribeToMarket/1,
	 unsubscribeFromMarket/1]).

-define(SERVER, ?MODULE).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {marketId}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(MarketId) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [MarketId], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% publish prices for a given market
%% @end
%%--------------------------------------------------------------------
-spec subscribeToMarket(integer()) -> ok.
subscribeToMarket(MarketId) ->
    httpc:request(put, {"http://rs.home:8000/market/" ++ integer_to_list(MarketId), [], "application/x-www-form-urlencoded", []}, [], []).

%%--------------------------------------------------------------------
%% @doc
%% stop publishing prices from a given market
%% @end
%%--------------------------------------------------------------------
-spec unsubscribeFromMarket(integer()) -> ok.
unsubscribeFromMarket(MarketId) ->
    httpc:request(delete, {"http://rs.home:8000/market/" ++ integer_to_list(MarketId), []}, [], []).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initializes the server
%%--------------------------------------------------------------------
init([MarketId]) ->
    process_flag(trap_exit, true),
    %%MarketId = bf_bot_util:get_marketId(),
    GatewayHost = bf_bot_util:get_gateway_host(),
  %  GatewayNode = bf_bot_util:get_gateway_node(),
    GatewayPort = bf_bot_util:get_gateway_port(),
%    TickkeeperNode = bf_bot_util:get_tickkeeper_node(),
  %  log4erl:info("gateway host: ~p, gateway port: ~p, MarketId: ~p", [GatewayHost, GatewayPort, MarketId]),    
    %% add the node to cluster - is there a better way of doing this?
 %   log4erl:info("connecting to nodes: ~p", [[GatewayNode, TickkeeperNode]]),
    %% pong = net_adm:ping(GatewayNode),
    %% pong = net_adm:ping(TickkeeperNode),
    %% timer:sleep(6000),
    %% init tickkeeper db
    %% log4erl:info("init tickkeeper db"),
    %% ok = init_tickkeeper(MarketId),
    log4erl:info("setting up connection to zeromq"),
    {ok, Context} = erlzmq:context(),
    {ok, Subscriber} = erlzmq:socket(Context, sub),
    ok = erlzmq:connect(Subscriber, "tcp://" ++ GatewayHost ++ ":" ++ integer_to_list(GatewayPort)),
    Filter = "{\"MarketId\":" ++ integer_to_list(MarketId),
    ok = erlzmq:setsockopt(Subscriber, subscribe, Filter),
    %% start a process to read the prices from 0MZ
    spawn_link(fun() -> loop(Subscriber, MarketId) end),
    %% send request to bf_gateway to start publishing prices for this MarketId
    log4erl:info("subscribing to marketId: ~p", [MarketId]),
    Reply = subscribeToMarket(MarketId),
    log4erl:info("subscribing to Market reply: ~p", [Reply]),
    {ok, #state{marketId = MarketId}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    unsubscribeFromMarket(State#state.marketId),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

loop(Subscriber, MarketId) ->
    {ok, Msg} = erlzmq:recv(Subscriber),
    io:format(Msg),
    %% tk_client:append(integer_to_list(MarketId),
    %% 		     {calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now())), 3.1345}),
    loop(Subscriber, MarketId).


%% init_tickkeeper(MarketId) ->
%%     case tk_client:open(integer_to_list(MarketId)) of
%% 	ok -> ok;
%% 	{error, _Err} -> tk_client:create(integer_to_list(MarketId), [{"timestamp", {integer, 64}}, {"ask", {float, 64}}])
%%     end.
		    
	    
