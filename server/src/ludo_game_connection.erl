-module(ludo_game_connection).
-behaviour(gen_server).

-include("include/records.hrl").
 
-export([start_link/1, send_packet/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link(Socket) ->
	gen_server:start_link(?MODULE, [Socket], []).
 
init([Socket]) ->
	io:format("started ~p~n", [self()]),
	gen_server:cast(self(), accept_connection),
	{ok, #client{socket=Socket}}.

send_packet(ConnectionPID, Packet) ->
	gen_server:cast(ConnectionPID, {packet, Packet}).

parse_buffer(Client = #client{inputBuffer=Buffer}, ExpectedLength) when length(Buffer) < ExpectedLength ->
	{Client, no_packet};

parse_buffer(Client = #client{inputBuffer=Buffer}, ExpectedLength) ->
	{Packet, UpdatedBuffer} = lists:split(ExpectedLength + 5, Buffer),
	BinaryPacket = iolist_to_binary(Packet),
	Client2 = Client#client{inputBuffer=UpdatedBuffer},
	{Client2, ludo_proto:parse(BinaryPacket)}.

parse_buffer(Client = #client{inputBuffer=Buffer}) when length(Buffer) < 5 ->
	{Client, no_packet};

parse_buffer(Client = #client{inputBuffer=Buffer}) ->
	<<ExpectedLength:(8 * 4)>> = binary:part(iolist_to_binary(Buffer), {1, 4}),
	parse_buffer(Client, ExpectedLength).

process_buffer(Client) ->
	{Client2, Packet} = parse_buffer(Client),
	case Packet of
		no_packet -> Client2;
		P ->
			gen_server:cast(self(), {receive_packet, P}),
			process_buffer(Client2)
	end.

process_data(Client = #client{inputBuffer=Buffer}, D) ->
	Buffer2 = lists:append([Buffer, D]),
	Client2 = Client#client{inputBuffer=Buffer2},
	process_buffer(Client2).

%% We never need you, handle_call!
handle_call(_, _, S) ->
	{noreply, S}.

handle_cast(accept_connection, Client = #client{socket=ListenSocket}) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	%%ludo_game_sup:start_socket(), % a new acceptor is born, praise the lord
	PlayerID = ludo_master:register_player(self(), 0),
	GameServerPID = ludo_master:find_game_by_id(0),
	gen_server:call(GameServerPID, {player_connected, PlayerID}),
	{noreply, Client#client{
		socket=AcceptSocket, 
		state=handshake, 
		id=PlayerID,
		gameServerPID=GameServerPID,
		inputBuffer=[]
	}};

handle_cast({packet, Packet}, Client = #client{socket=Socket}) ->
	gen_tcp:send(Socket, ludo_proto:compose(Packet)),
	{noreply, Client};

handle_cast({receive_packet, Packet}, Client = #client{gameServerPID=GameServerPID, id=PlayerID}) ->
	gen_server:cast(GameServerPID, {packet, PlayerID, Packet}),
	{noreply, Client}.

handle_info({tcp_closed, _Socket}, Client) ->
	io:format("CLOSING~n"),
	gen_server:cast(Client#client.gameServerPID, {player_disconnected, Client#client.id}),
	{stop, normal, Client};

handle_info({tcp_error, _Socket, _}, S) ->
	io:format("ERROR~n"),
	{stop, normal, S};

%% catch all packets
handle_info({_, _, D}, Client) ->
	Client2 = process_data(Client, D),
	{noreply, Client2}.

code_change(_OldVsn, Client, _Extra) ->
	{ok, Client}.
 
terminate(normal, _State) ->
	ok;
terminate(_Reason, _State) ->
	io:format("terminate reason: ~p~n", [_Reason]).
