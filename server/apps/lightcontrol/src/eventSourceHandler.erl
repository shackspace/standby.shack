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
        {loop, Req2, undefined, hibernate}.

info({event,_Event}, Req, State) ->
	io:format("Event: ~p~n",[_Event]),
	Data = "Da ist was",
        ok = cowboy_req:chunk(["id: ", Data, "\ndata: ", "Msg", "\n\n"], Req),
        {loop, Req, State}.

terminate(_Reason, _Req, _State) ->
        ok. 
