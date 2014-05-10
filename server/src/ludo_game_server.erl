-module(ludo_game_server).
-behaviour(gen_server).

-export([start_link/0]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

-include("include/records.hrl").

-record(gameState, {
	id,
	state,
	players
}).

-record(player, {
	id,
	pid,
	name,
	entityId
}).

%% API.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

add_player(State = #gameState{players=Players}, Player) ->
	State#gameState{players=[Player|Players]}.

find_player_by_id(#gameState{players=Players}, PlayerID) ->
	FilteredPlayers = [P || P = #player{id=ID} <- Players, PlayerID == ID],
	case FilteredPlayers of
		[] -> not_found;
		[Player] -> Player
	end.

broadcast(Packet, Players) ->
	[ludo_game_connection:send_packet(PlayerPID, Packet) || #player{pid=PlayerPID} <- Players].

broadcast(Packet, Players, Exclude) ->
	FilteredPlayers = [P || P <- Players, P#player.id =/= Exclude],
	broadcast(Packet, FilteredPlayers).

handle_packet(State, PlayerID, P = {move_packet, _D}) ->
	% TODO: update the entity
	broadcast(P, State#gameState.players, PlayerID),
	State;

handle_packet(State, PlayerID, {chat_packet, {Text}}) ->
	FormattedText = string:concat(io_lib:format("Player ~p: ", [PlayerID]), Text),
	broadcast({chat_packet, {FormattedText}}, State#gameState.players),
	State;

handle_packet(State, _PlayerID, _P) ->
	%% invalid packet received
	%% TODO: add some error logging
	State.

%% gen_server.
init([]) ->
	ServerID = ludo_master:register_game(self()),
	GameState = #state{
		worldBoundsX = 0.0, %% world boundaries: x
		worldBoundsY = 0.0, %% world boundaries: y
		worldBoundsWidth = 1280.0, %% world boundaries: width
		worldBoundsHeight = 1024.0, %% world boundaries: width
		entities = [],
		entityCount = 0
	},
	{ok, #gameState{id=ServerID, players=[], state=GameState}}.

handle_call({player_connected, PlayerID}, _From, State = #gameState{state=GameState, players=Players}) ->
	{PlayerPID, _} = ludo_master:find_player_by_id(PlayerID),
	EntityData = #entity{
		controller = "ludowars.controller.PlayerController", %% controller name
		representation = "ludowars.view.PlayerRepresentation", %% representation name
		driver = "ludowars.controller.EntityDriver", %% driver name
		positionX = 356.0 + 64 * GameState#state.entityCount, %% X
		positionY = 356.0 + 64 * GameState#state.entityCount, %% Y
		velocityX =	0.0, %% velocity X
		velocityY =	0.0, %% velocity Y
		angle =	0.0, %% angle
		width =	16, %% width
		height = 10 %% height
	},
	{EntityID, NewGameState} = ludo_game_state:add_entity(GameState, EntityData),
	NewEntityData = ludo_game_state:find_entity_by_id(NewGameState, EntityID),
	NewState = add_player(State, #player{
		id=PlayerID,
		pid=PlayerPID,
		entityId=EntityID
	}),
	ludo_game_connection:send_packet(PlayerPID, {state_packet, {NewGameState}}),
	ludo_game_connection:send_packet(PlayerPID, {assign_entity_packet, {EntityID}}),
	ludo_game_connection:send_packet(PlayerPID, {chat_packet, {io_lib:format("Hello! Your ID is ~p.", [PlayerID])}}),
	broadcast({add_entity, {NewEntityData}}, Players),
	{reply, success, NewState#gameState{state=NewGameState}};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({player_disconnected, PlayerID}, State = #gameState{state=GameState, players=Players}) ->
	Player = find_player_by_id(Players, PlayerID),
	EntityData = ludo_game_state:find_entity_by_id(GameState, Player#player.entityId),
	NewGameState = ludo_game_state:delete_entity(GameState, EntityData#entity.id),

	broadcast({delete_entity, EntityData#entity.id}, Players),
	{noreply, State#gameState{state=NewGameState}};

handle_cast({packet, PlayerID, Packet}, State) ->	
	State = handle_packet(State, PlayerID, Packet),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.