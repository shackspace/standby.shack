%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%% rest version 1
%%% @end
%%%-------------------------------------------------------------------

-module(restV2handler).

-export([init/3,
	allowed_methods/2,
	content_types_provided/2,
	content_types_accepted/2,
	textPlainPut/2,
	textPlainGet/2,
	applicationJsonPut/2,
	applicationJsonGet/2]).


%%--------------------------------------------------------------------
%% @doc
%% upgrade request to rest
%%
%% @spec init(_Transport, _Req, []) -> {upgrade, protocol, cowboy_rest}
%% @end
%%--------------------------------------------------------------------
init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.


%%--------------------------------------------------------------------
%% @doc
%% allow GET and PUT requests
%%
%% @spec allowed_methods(Req, State) -> {[<<"GET">>, <<"PUT">>], Req, State}
%% @end
%%--------------------------------------------------------------------
allowed_methods(Req, State) ->
	{[<<"GET">>, <<"PUT">>], Req, State}.


%%--------------------------------------------------------------------
%% @doc
%% mime type specific handlers, to handle GET requests
%%
%% @spec content_types_provided(Req, State) -> {[{{<<"text">>, <<"plain">>, []}, textPlain}], Req, State}
%% @end
%%--------------------------------------------------------------------
content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, '*'}, applicationJsonGet},
		{{<<"text">>, <<"plain">>, '*'}, textPlainGet}
	], Req, State}.


%%--------------------------------------------------------------------
%% @doc
%% mime type specific handlers for PUT requests
%% 
%% @spec content_types_accepted(Req, State) -> {[{{<<"text">>, <<"plain">>, []}, textPlain}], Req, State}
%% @end
%%--------------------------------------------------------------------
content_types_accepted(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, '*'}, applicationJsonPut},
		{{<<"text">>, <<"plain">>, '*'}, textPlainPut}
	], Req, State}.


%%%-------------------------------------------------------------------
%%% REST handlers
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% plaintext get handler
%%
%% @spec textPlainGet(Req, State) -> data
%% @end
%%--------------------------------------------------------------------
textPlainGet(Req, State) ->
	Data = getLightPlainText(getLightID(Req)),
	{Data, Req, State}.


%%--------------------------------------------------------------------
%% @doc
%% plain text put handler
%%
%% @spec textPlainPut(Req, State) -> true
%% @end
%%--------------------------------------------------------------------
textPlainPut(Req, State) ->
	{ok, [{NewStateBin,_}], _} = cowboy_req:body_qs(5,Req),
	if
		NewStateBin =:= <<"t">> ->
			toggleLight(getLightID(Req));
		true ->
			{NewState, _} = string:to_integer(binary_to_list(NewStateBin)),
			setLight(getLightID(Req), NewState)
	end,
	{true, Req, State}.


%%--------------------------------------------------------------------
%% @doc
%% application/json get handler
%%
%% @spec applicationJsonGet(Req, State) -> {Data, Req, State}
%% @end
%%--------------------------------------------------------------------
applicationJsonGet(Req, State) ->
	Data = getLightJson(getLightID(Req)),
	{Data, Req, State}.


%%--------------------------------------------------------------------
%% @doc
%% application/json put handler
%%
%% @spec applicationJsonPut(Req, State) -> {true, Req, State}
%% @end
%%--------------------------------------------------------------------
applicationJsonPut(Req, State) ->
	{ok, [{DataBin,_}], _} = cowboy_req:body_qs(200,Req),
	{ok, {Data}} = json:decode(DataBin),
	case proplists:get_value(<<"type">>, Data) of
		<<"switchOn">> ->
			setLight(getLightID(Req), 1);
		<<"switchOff">> ->
			setLight(getLightID(Req), 0);
		<<"set">> ->
			setLight(getLightID(Req), proplists:get_value(<<"state">>, Data))
	end,
	{true, Req, State}.


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% converts url path to light id
%%
%% @spec getLightID(Req) -> ID | all | error
%% @end
%%--------------------------------------------------------------------
getLightID(Req) ->
	{Path, _} = cowboy_req:path_info(Req),
	case Path of
		[<<"light">>] ->
			all;
		[<<"light">>, IDstr] ->
			{ID, _} = string:to_integer(binary_to_list(IDstr)),
			ID;
		_ ->
			error
	end.


%%--------------------------------------------------------------------
%% @doc
%% set state of light
%%
%% @spec setLight(ID, State) -> ok | error
%% @end
%%--------------------------------------------------------------------
setLight(ID, State) ->
	io:format("set light ~p to ~p~n", [ID, State]),
	ok.

%%--------------------------------------------------------------------
%% @doc
%% toggle light
%%
%% @spec toggleLight(ID) -> ok | error
%% @end
%%--------------------------------------------------------------------
toggleLight(ID) ->
	io:format("toggle light ~p~n", [ID]),
	ok.

%%--------------------------------------------------------------------
%% @doc
%% get light status
%%
%% @spec getLight(ID) -> State | error
%% @end
%%--------------------------------------------------------------------
getLight(ID) ->
	io:format("get light status ~p~n", [ID]),
	case ID of
		all ->
			[{120,1},{121,0},{122,0}];
		_ ->
			[{119,0}]
	end.

%%--------------------------------------------------------------------
%% @doc
%% get light state as plain text
%%
%% @spec getLightPlainText(ID) -> string()
%% @end
%%--------------------------------------------------------------------
getLightPlainText(ID) ->
	getLightPlainText(getLight(ID), "").

getLightPlainText([], StatesAsText) ->
	StatesAsText;

getLightPlainText(States, StatesAsText) ->
	[{ID, State}|Rest] = States,
	NewStatesAsText = lists:concat([StatesAsText, ID, " ", State, "\n"]),
	getLightPlainText(Rest, NewStatesAsText).


%%--------------------------------------------------------------------
%% @doc
%% get light as json
%%
%% @spec getLightJson(ID) -> string()
%% @end
%%--------------------------------------------------------------------
getLightJson(ID) ->
	{ok, Data} = json:encode({getLightJson(getLight(ID),[{"type", <<"states">>}])}),
	Data.

getLightJson([], DataPreEncode) ->
	DataPreEncode;

getLightJson(Data, DataPreEncode) ->
	[{ID, State}|Rest] = Data,
	NewDataPreEncode = lists:concat([DataPreEncode,[{integer_to_list(ID), State}]]),
	getLightJson(Rest, NewDataPreEncode).

