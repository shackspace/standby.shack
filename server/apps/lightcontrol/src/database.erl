%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(database).

-export([init/0,
	 getRealID/1,
	 getID/1,
	 updateState/2,
	 getState/1,
	 getPossibleState/1,
	 getAllID/0]).

-record(idmap,{id,realid}).
-record(state,{id,state}).
-record(possibleState,{id, state=[]}).
-record(addressid,{id,a1,a2,a3}).

%%--------------------------------------------------------------------
%% @doc
%% init tables
%%
%% @spec init() -> ok | error
%% @end
%%--------------------------------------------------------------------
init() ->
	mnesia:delete_table(idmap),
	mnesia:delete_table(state),
	mnesia:delete_table(possibleState),
	mnesia:delete_table(addressid),
	{atomic,ok} = mnesia:create_table(idmap, [
			{attributes, record_info(fields, idmap)},
			{disc_copies, [node()]}
			]),
	{atomic,ok} = mnesia:create_table(state, [
			{attributes, record_info(fields, state)},
			{disc_copies, [node()]}
			]),
	{atomic,ok} = mnesia:create_table(possibleState, [
			{attributes, record_info(fields, possibleState)},
			{disc_copies, [node()]}
		]),
	{atomic,ok} = mnesia:create_table(addressid, [
			{attributes, record_info(fields, addressid)},
			{disc_copies, [node()]}
			]),
	ok = init(
[        100       ,       101        ,       102        ,       103        ,       104        ,       105        ,       106        ,       107        ,       108        ,       109        ,       110        ,       111        ,       112        ,       113        ,       114        ,       115        ,       116        ,       117        ,       118        ,       119        ,       120        ,       121        ,       122        ,       123        ,       124        ,       125        ,       126],
[         26       ,        27        ,        26        ,        27        ,        28        ,        28        ,        28        ,        28        ,        29        ,        29        ,        29        ,        29        ,        30        ,        30        ,        30        ,        24        ,        24        ,        25        ,        25        ,        20        ,        21        ,        22        ,        23        ,        20        ,        21        ,        22        ,        23],
[["lounge","16","4"],["lounge","16","3"],["lounge","16","2"],["lounge","16","1"],["lounge","15","4"],["lounge","15","3"],["lounge","15","2"],["lounge","15","1"],["lounge","14","4"],["lounge","14","3"],["lounge","14","2"],["lounge","14","1"],["lounge","19","1"],["lounge","18","1"],["lounge","17","1"],["lounge","13","2"],["lounge","13","1"],["lounge","12","2"],["lounge","12","1"],["lounge","11","4"],["lounge","11","3"],["lounge","10","4"],["lounge","10","3"],["lounge","11","2"],["lounge","11","1"],["lounge","10","2"],["lounge","10","1"]]
	).

init([],[],[]) ->
	ok;
init(IDs, RealIDs, Addresses) ->
	[ID|RestID]=IDs,
	[RealID|RestRealIDs]=RealIDs,
	[Address|RestAddresses]=Addresses,
	[A1|A23]=Address,
	[A2|[A3]]=A23,
	io:format("~p ~p~n",[ID,RealID]),
	{atomic, ok} = mnesia:transaction(
		fun() ->
			mnesia:write(#idmap{id=ID,realid=RealID}),
			mnesia:write(#state{id=ID,state=0}),
			mnesia:write(#possibleState{id=ID,state=[0,1]}),
			mnesia:write(#addressid{id=ID,a1=A1,a2=A2,a3=A3})
		end),
	init(RestID,RestRealIDs,RestAddresses).


%%--------------------------------------------------------------------
%% @doc
%% get real ids, based on ID(fake ID)
%%
%% @spec getRealID(ID) -> [RealID,...] | error
%% @end
%%--------------------------------------------------------------------
getRealID(ID) ->
	case mnesia:dirty_read(idmap, ID) of
		[{idmap,ID,RealID}] ->
			RealID;
		_ ->
			error
	end.
	


%%--------------------------------------------------------------------
%% @doc
%% get ID(s)(fake ID(s)) which match Address
%%
%% @spec getID(Address) -> [ID,...] | error
%% @end
%%--------------------------------------------------------------------
getID(Address) when is_list(Address)->
	Match = case Address of
		[A1] ->
			{addressid,'_',A1,'_','_'};
		[A1,A2] ->
			{addressid,'_',A1,A2,'_'};
		[A1,A2,A3] ->
			{addressid,'_',A1,A2,A3};
		_ ->
			error
	end,
	Data = case Match of
		error ->
			error;
		Match ->
			mnesia:dirty_match_object(Match)
	end,
	getID(Data, []);

%%--------------------------------------------------------------------
%% @doc
%% get the ID(fake ID), based on RealID
%%
%% @spec getID(RealID) -> [ID,...] | error
%% @end
%%--------------------------------------------------------------------
getID(RealID) ->
	Data = mnesia:dirty_match_object({idmap,'_',RealID}),
	getID(Data, []).
getID([], Out) ->
	Out;
getID(IN, Out) ->
	{NID,NRest} = case IN of
		[{idmap,ID,_}|Rest] ->
			{ID,Rest};
		[{addressid,ID,_,_,_}|Rest] ->
			{ID,Rest}
	end,
	NewOut = Out ++ [NID],
	getID(NRest, NewOut).	


%%--------------------------------------------------------------------
%% @doc
%% update the state of light ID(fake ID) in database
%%
%% @spec updateState(ID, State) -> ok | error
%% @end
%%--------------------------------------------------------------------
updateState(ID, State) ->
	case mnesia:transaction(
		fun() ->
			mnesia:write(#state{id=ID, state=State})
		end)
	of
		{atomic, ok} ->
			ok;
		_ ->
			error
	end.

%%--------------------------------------------------------------------
%% @doc
%% get light state of ID(fake ID)
%%
%% @spec getState(ID) -> State | error
%% @end
%%--------------------------------------------------------------------
getState(ID) ->
	case mnesia:dirty_read(state, ID) of
		[{state,ID,State}] ->
			State;
		_ ->
			error
	end.


%%--------------------------------------------------------------------
%% @doc
%% get possible states for ID
%%
%% @spec getPossibleState(ID) -> [State,...] | error
%% @end
%%--------------------------------------------------------------------
getPossibleState(ID) ->
	case mnesia:dirty_read(possibleState, ID) of
		[{possibleState,ID,State}] ->
			State;
		_ ->
			error
	end.


%%--------------------------------------------------------------------
%% @doc
%% get all light ids
%%
%% @spec getAllID() -> [ID, ...] | error
%% @end
%%--------------------------------------------------------------------
getAllID() ->
	mnesia:dirty_all_keys(idmap).

