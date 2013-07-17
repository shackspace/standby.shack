%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(jsonHandler).

-export([getLight/1,
	 setLight/2]).


%%--------------------------------------------------------------------
%% @doc
%% returns the current light state as a json binary string
%%
%% @spec getLight(ID) -> json() | {error, Error}
%% @end
%%--------------------------------------------------------------------
getLight(ID) ->
	try
		json:encode(
			{getLight(mainServer:getLight(ID),[{"type", <<"states">>}])}
		)
	of
		{ok, Data} -> Data
	catch
		EType:Error -> {EType,Error}
	end.

getLight([], DataPreEncode) ->
	DataPreEncode;

getLight(Data, DataPreEncode) ->
	[{ID, State}|Rest] = Data,
	NewDataPreEncode = DataPreEncode ++ [{integer_to_list(ID), State}],
	getLight(Rest, NewDataPreEncode).
	

%%--------------------------------------------------------------------
%% @doc
%% decode a json binary string and tries to execute commands in there
%%
%% @spec setLight(JSON) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
setLight(JSON, ID) ->
	try
		{ok, {Data}} = json:decode(JSON),
		case proplists:get_value(<<"type">>, Data) of
			<<"switchOn">> ->
				mainServer:setLight(ID, 1);
			<<"switchOff">> ->
				mainServer:setLight(ID, 0);
			<<"set">> ->
				mainServer:setLight(ID, proplists:get_value(<<"state">>, Data))
		end
	of
		_ -> ok
	catch
		EType:Error -> {EType,Error}
	end.
	
