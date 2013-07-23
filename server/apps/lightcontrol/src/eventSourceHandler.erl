%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eventSourceHandler).

-export([init/3,
	 info/3,
	 terminate/3]).

init(_Transport, Req, []) ->
        Headers = [{<<"content-type">>, <<"text/event-stream">>}],
	mainServer:addListener(self()),
        {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),
        {loop, Req2, 1, hibernate}.

info({event,{updateLight,LightID,_LightState}}, Req, ID) ->
	{_,Msg} = jsonHandler:getLight(LightID),
        ok = cowboy_req:chunk(["id: ", integer_to_list(ID), "\ndata: ", Msg, "\n\n"], Req),
        {loop, Req, ID+1, hibernate};
info({event,{powerEvent,Power}}, Req, ID) ->
	{_,Msg} = jsonHandler:encodePower({powerEvent,Power}),
        ok = cowboy_req:chunk(["id: ", integer_to_list(ID), "\ndata: ", Msg, "\n\n"], Req),
        {loop, Req, ID+1, hibernate};
info(_, Req, ID) ->
	{loop, Req, ID, hibernate}.

terminate(_Reason, _Req, _ID) ->
	mainServer:removeListener(self()),
        ok. 
