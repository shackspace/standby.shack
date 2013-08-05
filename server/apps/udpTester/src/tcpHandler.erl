%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcpHandler).

-export([start_link/4, init/4]).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
	ok = ranch:accept_ack(Ref),
	loop(Socket, Transport).

loop(Socket, Transport) ->
	{_,TimeStamp,_} = erlang:now(),

	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	P1=integer_to_binary((random:uniform(350) + 1100)),
	P2=integer_to_binary((random:uniform(200) + 1000)),
	P3=integer_to_binary((random:uniform(900) + 1200)),

	Transport:send(Socket, <<"                    ">>),
	Transport:send(Socket, integer_to_binary(TimeStamp)), Transport:send(Socket, <<"          \r\n">>),
	Transport:send(Socket, <<"HAG5eHZ010C_IEnBWA02\r\n">>),
	Transport:send(Socket, <<"2\r\n">>),
	Transport:send(Socket, <<"1-0:0.0.0*255(20745965)2\r\n">>),
	Transport:send(Socket, <<"1-0:1.8.0*255(011107.1314)2\r\n">>),
	Transport:send(Socket, <<"1-0:96.5.5*255(82)2\r\n">>),
	Transport:send(Socket, <<"1-0:32.7.0*255(233.90*V)2\r\n">>),
	Transport:send(Socket, <<"1-0:52.7.0*255(233.07*V)2\r\n">>),
	Transport:send(Socket, <<"1-0:72.7.0*255(236.50*V)2\r\n">>),
	Transport:send(Socket, <<"1-0:31.7.0*255(004.99*A)2\r\n">>),
	Transport:send(Socket, <<"1-0:51.7.0*255(005.02*A)2\r\n">>),
	Transport:send(Socket, <<"1-0:71.7.0*255(007.14*A)2\r\n">>),
	Transport:send(Socket, <<"1-0:21.7.0*255(+">>), Transport:send(Socket, P1), Transport:send(Socket, <<"      \r\n">>),
	Transport:send(Socket, <<"1-0:21.7.0*255(+">>), Transport:send(Socket, P2), Transport:send(Socket, <<"      \r\n">>),
	Transport:send(Socket, <<"1-0:21.7.0*255(+">>), Transport:send(Socket, P3), Transport:send(Socket, <<"      \r\n">>),
	Transport:send(Socket, <<"1-0:96.50.0*0(EF)2\r\n">>),
	Transport:send(Socket, <<"1-0:96.50.0*1(07CE)2\r\n">>),
	Transport:send(Socket, <<"1-0:96.50.0*2(10)2\r\n">>),
	Transport:send(Socket, <<"1-0:96.50.0*3(0B)2\r\n">>),
	Transport:send(Socket, <<"1-0:96.50.0*4(28)2\r\n">>),
	Transport:send(Socket, <<"1-0:96.50.0*5(1D)2\r\n">>),
	Transport:send(Socket, <<"1-0:96.50.0*6(003D381B260A16F1F6FE560200009F80)2\r\n">>),
	Transport:send(Socket, <<"1-0:96.50.0*7(00)2\r\n">>),
	Transport:send(Socket, <<"!2\r\n">>),
	Transport:close(Socket).
%%	case Transport:recv(Socket, 0, 5000) of
%%		{ok, Data} ->
%%			Transport:send(Socket, Data),
%%			loop(Socket, Transport);
%%		_ ->
%%			ok = Transport:close(Socket)
%%	end.
