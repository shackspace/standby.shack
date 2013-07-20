%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(plainTextHandler).

-export([getLight/1,
	 setLight/2]).

%%--------------------------------------------------------------------
%% @doc
%% get light state as plain text
%%
%% @spec getLightPlainText(ID) -> {ok, string()}
%% @end
%%--------------------------------------------------------------------
getLight(ID) ->
	try
		getLight(mainServer:getLight(ID), "")
	of
		Data ->
			{ok, Data}
	catch
		_:_ ->
			{error, "can't get lights"}
	end.


getLight([], StatesAsText) ->
	StatesAsText;

getLight(States, StatesAsText) ->
	[{ID, State}|Rest] = States,
	NewStatesAsText = lists:concat([StatesAsText, ID, " ", State, "\n"]),
	getLight(Rest, NewStatesAsText).


%%--------------------------------------------------------------------
%% @doc
%% set light state
%%
%% @spec setLight(ID, Data) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
setLight(ID, Data) ->
	try
		if
			Data =:= <<"t">> ->
				mainServer:toggleLight(ID);
			true ->
				case string:to_integer(binary_to_list(Data)) of
					{error, Error} ->
						{error, Error};
					{NewState, _} ->
						mainServer:setLight(ID, NewState)
				end
		end
	of
		Result -> Result
	catch
		EType:Error2 -> {EType,Error2}
	end.
