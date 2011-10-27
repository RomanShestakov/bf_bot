%%%-------------------------------------------------------------------
%%% File    : bf_bot.erl
%%% Author  : Roman Shestakov <>
%%% Description : 
%%%
%%% Created : 23 Oct 2011 by Roman Shestakov <>
%%%-------------------------------------------------------------------
-module(bf_bot).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 subscribeMarket/1,
	 unsubscribeMarket/1]).

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
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% publish prices for a given market
%% @end
%%--------------------------------------------------------------------
-spec subscribeMarket(integer()) -> ok.
subscribeMarket(MarketId) ->
    gen_server:cast({global, bf_gateway}, {subscribeMarket, MarketId}).

%%--------------------------------------------------------------------
%% @doc
%% stop publishing prices from a given market
%% @end
%%--------------------------------------------------------------------
-spec unsubscribeMarket(integer()) -> ok.
unsubscribeMarket(MarketId) ->
    gen_server:cast({global, bf_gateway}, {unsubscribeMarket, MarketId}).


%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initializes the server
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    log4erl:info("entered init"),    
    MarketId = bf_bot_util:get_marketId(),
    GatewayHost = bf_bot_util:get_gateway_host(),
    GatewayPort = bf_bot_util:get_gateway_port(),
    log4erl:info("gateway host: ~p, gateway port: ~p, MarketId: ~p", [GatewayHost, GatewayPort, MarketId]),    
    log4erl:info("setting up connection to zeromq"),
    {ok, Context} = erlzmq:context(),
    {ok, Subscriber} = erlzmq:socket(Context, sub),
    ok = erlzmq:connect(Subscriber, "tcp://" ++ GatewayHost ++ ":" ++ integer_to_list(GatewayPort)),
    Filter = "{\"MarketId\":" ++ integer_to_list(MarketId),
    ok = erlzmq:setsockopt(Subscriber, subscribe, Filter),
    %% start a process to read the prices from 0MZ
    spawn_link(fun() -> loop(Subscriber) end),
    %% send request to bf_gateway to start publishing prices for this MarketId
    ok = subscribeMarket(MarketId),
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
    unsubscribeMarket(State#state.marketId),
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

loop(Subscriber) ->
    {ok, Msg} = erlzmq:recv(Subscriber),
    io:format(Msg),
    loop(Subscriber).
