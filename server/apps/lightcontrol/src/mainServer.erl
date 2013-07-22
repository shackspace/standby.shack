%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mainServer).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 getLight/1,
	 setLight/2,
	 toggleLight/1,
	 updateRealLight/2,
	 getRealLight/1,
	 addListener/1,
	 sendEvent/1]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {listener=[]}).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% send an event to all regestrated listeners
%%
%% @spec sendEvent(Event) -> ok | error
%% @end
%%--------------------------------------------------------------------
sendEvent(Event) ->
	?MODULE ! {event, Event},
	ok.

%%--------------------------------------------------------------------
%% @doc
%% add an event listener
%%
%% @spec addListener(PID) -> ok | error
%% @end
%%--------------------------------------------------------------------
addListener(PID) when is_pid(PID) ->
	?MODULE ! {addListener, PID},
	ok;
addListener(_) ->
	error.

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
%% get light status
%%
%% @spec getLight(ID) -> [{ID,State},...] | error
%% @end
%%--------------------------------------------------------------------
getLight(ID) ->
	light:getLight(ID).


%%--------------------------------------------------------------------
%% @doc
%% set light
%%
%% @spec setLight(ID, State) -> ok | error
%% @end
%%--------------------------------------------------------------------
setLight(ID, State) ->
	io:format("set light ~p to ~p~n", [ID, State]),
	light:setLight(ID, State).


%%--------------------------------------------------------------------
%% @doc
%% toggle light
%%
%% @spec toggleLight(ID) -> ok | error
%% @end
%%--------------------------------------------------------------------
toggleLight(ID) ->
	light:toggleLight(ID).


%%--------------------------------------------------------------------
%% @doc
%% get light state based on real id
%%
%% @spec getRealLight(RealID) -> State | error
%% @end
%%--------------------------------------------------------------------
getRealLight(RealID) ->
	light:getRealLight(RealID).

%%--------------------------------------------------------------------
%% @doc
%% update light state in database, based on the RealID
%%
%% @spec updateRealLight(RealID, State) -> ok | error
%% @end
%%--------------------------------------------------------------------
updateRealLight(RealID, State) ->
	light:updateRealLight(RealID, State).

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
	State = #state{},
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({event, Event}, State) ->
	NewListener = emitEvent(State#state.listener, [], Event),
	NewState = State#state{listener=NewListener},
	{noreply, NewState};
handle_info({addListener, PID}, State) ->
	Listener=State#state.listener,
	NewState=State#state{listener=[PID|Listener]},
	{noreply, NewState};
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
%% send event to listeners
%%
%% @spec emitEvent(Listener,NewListener,Event) -> NewListener | error
%% @end
%%--------------------------------------------------------------------
emitEvent([], NewListener, _Event) ->
	NewListener;
emitEvent(Listeners, NewListener, Event) ->
	[Listener|Rest] = Listeners,
	NewNewListener = case is_process_alive(Listener) of
		true ->
			Listener ! {event, Event},
			[Listener|NewListener];
		_ ->
			NewListener
	end,
	emitEvent(Rest, NewNewListener, Event).
