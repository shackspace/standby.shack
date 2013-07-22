%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%% rest version 1
%%% @end
%%%-------------------------------------------------------------------

-module(restV1handler).

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
%% @spec allowed_methods(Req, State) -> {AllowedMethods, Req, State}
%% @end
%%--------------------------------------------------------------------
allowed_methods(Req, State) ->
	{[<<"GET">>, <<"PUT">>], Req, State}.


%%--------------------------------------------------------------------
%% @doc
%% mime type specific handlers, to handle GET requests
%%
%% @spec content_types_provided(Req, State) -> {ProvidedContentTypes, Req, State}
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
%% @spec content_types_accepted(Req, State) -> {AcceptedContentTypes, Req, State}
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
	Data = case convertURL_ID(Req) of
		error ->
			<<"can't convert url">>;
		ID ->
			{_, Lights} = plainTextHandler:getLight(ID),
			Lights
	end,
	{Data, Req, State}.


%%--------------------------------------------------------------------
%% @doc
%% plain text put handler
%%
%% @spec textPlainPut(Req, State) -> true
%% @end
%%--------------------------------------------------------------------
textPlainPut(Req, State) ->
	{ok, [{NewStateBin,_}], _} = cowboy_req:body_qs(200,Req),
	Result = case convertURL_ID(Req) of
		error ->
			false;
		ID ->
			plainTextHandler:setLight(ID, NewStateBin) =:= ok  %in reason on error, return false
	end,
	{Result, Req, State}.


%%--------------------------------------------------------------------
%% @doc
%% application/json get handler
%%
%% @spec applicationJsonGet(Req, State) -> {Data, Req, State}
%% @end
%%--------------------------------------------------------------------
applicationJsonGet(Req, State) ->
	Data = case convertURL_ID(Req) of
		error ->
			<<"{\"type\":\"error\",\"error\":\"can't convert url\"}">>;
		ID ->
			{_, Lights} = jsonHandler:getLight(ID),
			Lights
	end,
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
	Result = case convertURL_ID(Req) of
		error ->
			false;
		ID ->
			jsonHandler:setLight(ID, DataBin) =:= ok  %in reason on error, return false
	end,
	{Result, Req, State}.


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% converts url path to light id
%%
%% @spec convertURL_ID(Req) -> ID | all | error
%% @end
%%--------------------------------------------------------------------
convertURL_ID(Req) ->
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

