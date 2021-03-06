%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(jsonHandler).

-export([getLight/1,
	 setLight/2,
	 getPowerLog/1,
	 encodePower/1]).


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
			{getLight(mainServer:getLight(ID),[{"type", <<"states">>}],0)}
		)
	of
		Result -> Result
	catch
		_:_ ->
			JSON = <<"{\"type\":\"error\",\"error\":\"can't get lights\"}">>,
			{error, JSON}
	end.

getLight([], DataPreEncode, IsON) ->
	DataPreEncode ++ [{"state", IsON}];
getLight([{ID, State}], [DataPreEncode], 0) ->
	[DataPreEncode,
		{"id", ID},
		{"state", State},
		{integer_to_list(ID), State},
		{"address", lists:map(fun(X) -> list_to_binary(X) end, mainServer:getAddress(ID))}
	];
getLight(Data, DataPreEncode, IsON) ->
	[{ID, State}|Rest] = Data,
	NewIsOn = case State of
		1 -> 1;
		_ -> IsON
	end,
	NewDataPreEncode = DataPreEncode ++ [{integer_to_list(ID), State}],
	getLight(Rest, NewDataPreEncode, NewIsOn).
	

%%--------------------------------------------------------------------
%% @doc
%% decode a json binary string and tries to execute commands in there
%%
%% @spec setLight(ID, JSON) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
setLight(ID, JSON) ->
	try
		{ok, {Data}} = json:decode(JSON),
		case proplists:get_value(<<"type">>, Data) of
			<<"switchOn">> ->
				mainServer:setLight(ID, 1);
			<<"switchOff">> ->
				mainServer:setLight(ID, 0);
			<<"toggle">> ->
				mainServer:toggleLight(ID);
			<<"set">> ->
				case proplists:get_value(<<"state">>, Data) of
					undefined ->
						{error, state_undefined};
					State when is_integer(State) ->
						mainServer:setLight(ID, State);
					_ ->
						{error, unknown_error}
				end
		end
	of
		Result -> Result
	catch
		EType:Error -> {EType,Error}
	end.


%%--------------------------------------------------------------------
%% @doc
%% encode power events
%%
%% @spec encodePower(Event) -> {ok,json()} | {error, json()}
%% @end
%%--------------------------------------------------------------------
encodePower({powerEvent,Data}) ->
	PreEncode = [{"type",<<"power">>}] ++ Data,
	try
		json:encode({PreEncode})
	of
		Result -> Result
	catch
		_:_ ->
			JSON = <<"{\"type\":\"error\",\"error\":\"can't parse power event\"}">>,
			{error, JSON}
	end;
encodePower(_) ->
	JSON = <<"{\"type\":\"error\",\"error\":\"can't parse power event\"}">>,
	{error, JSON}.


%%--------------------------------------------------------------------
%% @doc
%% get last N powerStates as JSON-String
%%
%% @spec getPowerLog(N) -> PowerLogAsJSON | error
%% @end
%%--------------------------------------------------------------------
getPowerLog(N) when is_integer(N) ->
	{Time,P1,P2,P3} = database:getPowerLog(N),
	json:encode({[{type, <<"powerLog">>},{time,Time},{p1,P1},{p2,P2},{p3,P3}]});
getPowerLog(_) ->
	JSON = <<"{\"type\":\"error\",\"error\":\"can't get powerLog\"}">>,
	{error, JSON}.
