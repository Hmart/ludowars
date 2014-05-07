
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
	{ok, #gameState{id=ServerID}}.

handle_call({player_connected, PlayerPID}, _From, State) ->
	ludo_game_connection:send_packet(PlayerPID, {statePacket,
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
	ludo_game_connection:send_packet(PlayerPID, {assignEntityPacket,
		1 %% entity ID
	}),
	{reply, success, State};

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