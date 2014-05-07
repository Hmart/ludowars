
-module(ludo_master).
-behaviour(gen_server).

-export([start_link/0, stop/0, register_game/1, register_player/2, find_game_by_id/1, find_player_by_id/1]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

-define(SERVER, ?MODULE).

-include("include/records.hrl").

-record(masterState, {
	players = [],
	games = [],
	serverCount = 0,
	playerCount = 0
}).

%% API.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:call(?SERVER, stop).

register_game(ServerPID) ->
	gen_server:call(?SERVER, {register_game, ServerPID}).

register_player(PlayerPID, ServerID) ->
	gen_server:call(?SERVER, {register_player, PlayerPID, ServerID}).

find_game_by_id(ServerID) ->
	gen_server:call(?SERVER, {find_game_by_id, ServerID}).

find_player_by_id(PlayerID) ->
	gen_server:call(?SERVER, {find_player_by_id, PlayerID}).

%% gen_server.
init([]) ->
	{ok, #masterState{}}.

handle_call({find_game_by_id, ServerID}, _From, #masterState{games = Games} = State) ->
	L = lists:keyfind(ServerID,1,Games),
	case L of 
		false -> {reply, undefined,State};
		{_, ServerPID} -> {reply, ServerPID, State}
	end;

handle_call({find_player_by_id, PlayerID}, _From, #masterState{players=Players} = State) ->
	L = lists:keyfind(PlayerID, 1, Players),
	case L of 
		false -> {reply, undefined, State};
		{_, PlayerPID, ServerPID} -> {reply, {PlayerPID, ServerPID}, State}
	end;

handle_call({register_game, ServerPID}, _From, #masterState{games=Games, serverCount=ServerCount} = State) ->
	ServerID = ServerCount,
	{reply, ServerID, State#masterState{
		games=[{ServerID, ServerPID} | Games], 
		serverCount=ServerCount + 1
	}};

handle_call({register_player, PlayerPID, ServerID}, _From, #masterState{players=Players, playerCount=PlayerCount} = State) ->
	PlayerID = PlayerCount, 
	{reply, PlayerID, State#masterState{
		players=[{PlayerID, PlayerPID, ServerID} | Players], 
		playerCount=PlayerCount + 1
	}};

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