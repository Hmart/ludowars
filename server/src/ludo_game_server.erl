
-module(ludo_game_server).
-behaviour(gen_server).

-export([start_link/0]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

-include("include/records.hrl").

-record(gameState, {
	id,
	state
}).

%% API.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

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
	{ok, #gameState{id=ServerID, state=GameState}}.

handle_call({player_connected, PlayerPID}, _From, #gameState{state=GameState} = State) ->
	{EntityID, NewGameState} = ludo_game_state:add_entity(GameState, #entity{
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
	}),
	ludo_game_connection:send_packet(PlayerPID, {statePacket, NewGameState}),
	ludo_game_connection:send_packet(PlayerPID, {assignEntityPacket, EntityID}),
	{reply, success, State#gameState{state=NewGameState}};

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