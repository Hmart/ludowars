-module(ludo_game_serv).
-behaviour(gen_server).

-include("include/records.hrl").
 
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link(Socket) ->
	gen_server:start_link(?MODULE, Socket, []).
 
init(Socket) ->
	gen_server:cast(self(), accept_connection),
	{ok, #client{socket=Socket}}.

%% We never need you, handle_call!
handle_call(_, _, S) ->
	{noreply, S}.

handle_cast(accept_connection, Client = #client{socket=ListenSocket}) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	ludo_game_sup:start_socket(), % a new acceptor is born, praise the lord
	ludo_proto:compose({statePacket,
		0.0, %% world boundaries: x
		0.0, %% world boundaries: y
		500.0, %% world boundaries: width
		500.0, %% world boundaries: width
		[
			{
				1, %% entity id
				"ludowars.controller.PlayerController", %% controller name
				"ludowars.view.PlayerRepresentation", %% representation name
				"ludowars.controller.EntityDriver", %% driver name
				356.0, %% X
				356.0, %% Y
				0.0, %% velocity X
				0.0, %% velocity Y
				0.0, %% angle
				16, %% width
				10 %% height
			}
		]
	}),
	{noreply, Client#client{socket=AcceptSocket, state=handshake}}.

handle_info({tcp_closed, _Socket}, S) ->
	io:format("CLOSING~n"),
	{stop, normal, S};

handle_info({tcp_error, _Socket, _}, S) ->
	io:format("ERROR~n"),
	{stop, normal, S};

%% catch all packets
handle_info({_, _, D}, Client) ->
	io:format("Data~p~n", [ludo_proto:parse(D)]),
	{noreply, Client}.

code_change(_OldVsn, Client, _Extra) ->
	{ok, Client}.
 
terminate(normal, _State) ->
	ok;
terminate(_Reason, _State) ->
	io:format("terminate reason: ~p~n", [_Reason]).
