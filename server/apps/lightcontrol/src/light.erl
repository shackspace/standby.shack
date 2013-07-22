%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(light).

-export([getLight/1,
	 setLight/2,
	 toggleLight/1,
	 updateRealLight/2,
	 updateLight/2,
	 getRealLight/1]).


%%--------------------------------------------------------------------
%% @doc
%% get light
%%
%% @spec getLight(ID) -> State | error
%% @end
%%--------------------------------------------------------------------
getLight(all) ->
	getLight(database:getAllID(),[]);
getLight(Address) when is_list(Address) ->
	case getLight(database:getID(Address),[]) of
		[] -> error;
		States -> States
	end;
getLight(ID) ->
	case database:getState(ID) of
		error ->
			error;
		State ->
			[{ID,State}]
	end.
getLight([], Result) ->
	Result;
getLight(IDs, Result) ->
	[ID|RestIDs] = IDs,
	NewResult = Result ++ [{ID,database:getState(ID)}],
	getLight(RestIDs, NewResult).


%%--------------------------------------------------------------------
%% @doc
%% set light state
%%
%% @spec setLight(ID, State) -> ok | error
%% @end
%%--------------------------------------------------------------------
setLight(all, State) ->
	setLightL(database:getAllID(), State);
setLight(Address, State) when is_list(Address) ->
	case database:getID(Address) of
		[] ->
			error;
		IDs when is_list(IDs) ->
			setLightL(IDs, State);
		_ ->
			error
	end;
setLight(ID, State) ->
	case database:getPossibleState(ID) of
		error ->
			error;
		AllowStates ->
			case lists:member(State, AllowStates) of
				true ->
					udpServer:setLight(database:getRealID(ID),State);
				_ ->
					error
			end
	end.
setLightL([], _State) ->
	ok;
setLightL(Lights, State) ->
	[ID|Rest] = Lights,
	case setLight(ID, State) of
		ok ->
			setLightL(Rest, State);
		_ ->
			error
	end.


%%--------------------------------------------------------------------
%% @doc
%% toggle light state
%%
%% @spec toggleLight(ID) -> ok | error
%% @end
%%--------------------------------------------------------------------
toggleLight(all) ->
	toggleLight(database:getAllID());
toggleLight(Address) when is_list(Address) ->
	case database:getID(Address) of
		[] ->
			error;
		IDs when is_list(IDs) ->
			toggleLightL(IDs);
		_ ->
			error
	end;
toggleLight(ID) ->
	case getLight(ID) of
		error ->
			error;
		[{ID,1}] ->
			setLight(ID, 0);
		[{ID,0}] ->
			setLight(ID, 1);
		_ ->
			error
	end.
toggleLightL([]) ->
	ok;
toggleLightL(Lights) ->
	[ID|Rest] = Lights,
	case toggleLight(ID) of
		ok ->
			toggleLightL(Rest);
		_ ->
			error
	end.


%%--------------------------------------------------------------------
%% @doc
%% get light state based on real id
%%
%% @spec getRealLight(RealID) -> State | error
%% @end
%%--------------------------------------------------------------------
getRealLight(RealID) ->
	case database:getID(RealID) of
		error ->
			error;
		IDs ->
			[ID|_] = IDs,
			case database:getState(ID) of
				error ->
					error;
				State ->
					State
			end
	end.

%%--------------------------------------------------------------------
%% @doc
%% update light state in database, based on the RealID
%%
%% @spec updateRealLight(RealID, State) -> ok | error
%% @end
%%--------------------------------------------------------------------
updateRealLight(RealID, State) ->
	case database:getID(RealID) of
		error ->
			error;
		Lights ->
			updateLight(Lights, State)
	end.

updateLight([], _State) ->
	ok;
updateLight(Lights, State) when is_list(Lights) ->
	[ID|Rest] = Lights,
	case updateLight(ID, State) of
		ok ->
			updateLight(Rest, State);
		_ ->
			error
	end;
updateLight(ID, State) ->
	case database:getPossibleState(ID) of
		error ->
			error;
		AllowStates ->
			case lists:member(State, AllowStates) of
				true ->
					database:updateState(ID, State);
				_ ->
					error
			end
	end.
