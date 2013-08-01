%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(udpServer).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 setLight/2]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {udpsocket, waiting=[]}).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	io:format(" *** ~p: start link~n~n", [?MODULE]),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% set the light state based on the RealID, and RealState
%%
%% @spec setLight(RealID, ToState) -> ok
%% @end
%%--------------------------------------------------------------------
setLight(RealID, ToState) ->
	%try 200 times to setLight
	?MODULE ! {set, RealID, ToState, 200},
	ok.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	io:format(" *** ~p: init:~n\tOpts='[]'~n~n", [?MODULE]),
	{ok, UDPSocket} = gen_udp:open(2342, [list, {active,once}, {broadcast,true}, {reuseaddr, true}]),
	State=#state{udpsocket=UDPSocket},
	{ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
	io:format(" *** ~p: unexpected call:~n\tRequest='~p', From='~p', State='~p'~n~n", [?MODULE, _Request, _From, State]),
	Reply = ok,
	{reply, Reply, State}.




%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	io:format(" *** ~p: unexpected cast:~n\tMsg='~p', State='~p'~n~n", [?MODULE, _Msg, State]),
	{noreply, State}.


%% @doc
%% starts a loop, to set light
%% @end
handle_info({set, RealID, ToState, Count}, State) ->
	NewState = case proplists:is_defined(RealID, State#state.waiting) of
		true ->
			io:format("waiting for ~p(~p)~n", [RealID, ToState]),
			erlang:send_after(100, ?MODULE, {set, RealID, ToState, Count-1}),
			State#state{waiting = State#state.waiting ++ [{RealID,ToState}]};
		false ->
			%try 200 times to set light state
			erlang:spawn_link(fun() -> testComplet(RealID, ToState, 200) end),
			State
	end,
	{noreply, NewState};

%% @doc
%% send Data to 'licht.shack' on port 1337
%% @end
handle_info({send, Data}, State) ->
	gen_udp:send(State#state.udpsocket, 'licht.shack', 1337, Data),
	{noreply, State};

%% @doc
%% recive udp package and update light state in database if
%% sender match 'licht.shack' 
%% @end
handle_info({udp, SourceSocket, From, 2342, [LightID, LightState]}, State) ->
	Socket = State#state.udpsocket,
	{ok, IP} = inet:getaddr('licht.shack', inet),
	NewState = case {SourceSocket, From} of
		{Socket, IP} ->
			mainServer:updateRealLight(LightID, LightState),
			State#state{waiting = State#state.waiting -- [{LightID, LightState}]}
	end,
	inet:setopts(Socket, [{active, once}]),
	{noreply, NewState};

%% @doc
%% unexpected info
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end

handle_info(_Info, State) ->
	io:format(" *** ~p: unexpected info:~n\tInfo='~p', State='~p'~n~n", [?MODULE, _Info, State]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	io:format(" *** ~p: terminate:~n\tReason='~p', State='~p'~n~n", [?MODULE, _Reason, _State]),
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	io:format(" *** ~p: code change:~n\tOldVsn='~p', State='~p', Extra='~p'~n~n", [?MODULE, _OldVsn, State, _Extra]),
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% send udp again as long state hasn't been set correctly
%%
%% @spec testComplet(_RealID, _ToState, RetryNumber) -> Reply
%% @end
%%--------------------------------------------------------------------
testComplet(_RealID, _ToState, 0) ->
	error;

testComplet(RealID, ToState, Count) ->
	?MODULE ! {send, [16#A5,16#5A,RealID,ToState]},
	timer:sleep(100),
	case mainServer:getRealLight(RealID) of
		ToState ->
			%io:format(" *** ~p:~n\tLight ~p is now ~p. ~n~n", [?MODULE, LightID, LightState]),
			ok;
		_ ->
			io:format("~p ",[Count]),
			testComplet(RealID, ToState, Count-1)
	end.
