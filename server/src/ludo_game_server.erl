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

broadcast(Packet, Players) ->
	[ludo_game_connection:send_packet(PlayerPID, Packet) || #player{pid=PlayerPID} <- Players].

broadcast(Packet, Players, Exclude) ->
	FilteredPlayers = lists:filter(
		fun(#player{pid=PlayerPID}) -> PlayerPID =/= Exclude end,
		Players
	),
	broadcast(Packet, FilteredPlayers).

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
	ludo_game_connection:send_packet(PlayerPID, {state_packet, NewGameState}),
	ludo_game_connection:send_packet(PlayerPID, {assign_entity_packet, EntityID}),
	broadcast({add_entity, NewEntityData}, Players),
	{reply, success, NewState#gameState{state=NewGameState}};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.