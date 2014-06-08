%%% @doc Ludo masterserver, This module defines a masterserver.
%%% @end

-module(ludo_master).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, register_game/0, register_player/1, find_game_by_id/1, find_player_by_id/1,
	find_player_by_pid/1, broadcast/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("include/records.hrl").

-record(masterState, {
	players = [],
	games = [],
	serverCount = 0,
	playerCount = 0
}).

%% API.

%% @doc Starts the server.
%%
%% @spec start_link() -> {ok, Pid}
%% where
%%	Pid = pid()
%% @end
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
stop() ->
	gen_server:call(?SERVER, stop).

%% @doc Registers a gameserver.
%% @spec register_game() -> {ok, ServerID::integer()}
%% @end
register_game() ->
	gen_server:call(?SERVER, {register_game, self()}).

%% @doc Registers a player.
%% @spec register_player(ServerID::integer()) -> {ok, {PlayerID, ServerPID}}
%% where
%%	PlayerID = integer()
%%  ServerPID = pid()
%% @end
register_player(ServerID) ->
	gen_server:call(?SERVER, {register_player, self(), ServerID}).

%% @doc Returns all players on a specific gameserver.
%% @spec get_players_by_server_id(ServerID::integer()) -> {ok, Players}
%% where
%%	Players = list()
%% @end
get_players_by_server_id(ServerID) ->
	gen_server:call(?SERVER, {players_by_server_id, ServerID}).

%% @spec find_game_by_id(ServerID::integer()) -> {ok, ServerPID::pid()}
%% @end
%% @doc Calls `find_game_by_id_(ServerID)'.

find_game_by_id(ServerID) ->
	gen_server:call(?SERVER, {find_game_by_id, ServerID}).

%% @doc Returns the playerPID and ServerPID connected to the PlayerID.
%% @spec find_player_by_id(PlayerID::integer()) -> {ok, {PlayerPID, ServerPID}}
%% where
%% 	PlayerPID = pid()
%% 	ServerPID = pid()
%% @end
find_player_by_id(PlayerID) ->
	gen_server:call(?SERVER, {find_player_by_id, PlayerID}).

%% @doc Returns the playerID connected to the playerPID.
%% @spec find_player_by_pid(PlayerPID::pid()) -> {ok, PlayerID}
%% where
%%	PlayerPID = pid()
%% @end
find_player_by_pid(PlayerPID) ->
	gen_server:call(?SERVER, {find_player_by_pid, PlayerPID}).

%% @doc Returns the playerID connected to the playerPID.
%% @spec find_player_by_id_(MasterState, ServerID::integer()) -> {ok, ServerPID}
%% where
%%	ServerPID = pid()
%% @end
find_game_by_id_(#masterState{games=Games}, ServerID) ->
	L = lists:keyfind(ServerID, 1, Games),
	case L of 
		false -> undefined;
		{_, ServerPID} -> ServerPID
	end.

%% @doc Broadcasts a message to all players connected to a server.
%% @spec broadcast(ServerID::integer(), Message) -> ok
%% @end
broadcast(ServerID, Message) ->
	Players = [PlayerPID || {_, PlayerPID} <- get_players_by_server_id(ServerID)],
 	io:format("broadcast players ~p~n", [Players]),

	[gen_server:cast(PlayerPID, Message) || PlayerPID <- Players].

%% gen_server callbacks.
init([]) ->
	{ok, #masterState{}}.

handle_call({players_by_server_id, ServerID}, _From, State) ->
	Players = [{PlayerID, PlayerPID} || {PlayerID, PlayerPID, PlayerServerID} 
		<- State#masterState.players, ServerID == PlayerServerID],
	{reply, Players, State};

handle_call({find_game_by_id, ServerID}, _From, State) ->
	{reply, find_game_by_id_(State, ServerID), State};

handle_call({find_player_by_id, PlayerID}, _From, #masterState{players=Players} = State) ->
	L = lists:keyfind(PlayerID, 1, Players),
	case L of 
		false -> {reply, undefined, State};
		{_, PlayerPID, ServerPID} -> {reply, {PlayerPID, ServerPID}, State}
	end;

handle_call({find_player_by_pid, PlayerPID}, _From, #masterState{players=Players} = State) ->
	L = lists:keyfind(PlayerPID, 2, Players),
	case L of 
		false -> {reply, undefined, State};
		{PlayerID, _, _} -> {reply, PlayerID, State}
	end;

handle_call({register_game, ServerPID}, _From, #masterState{games=Games, serverCount=ServerCount} = State) ->
	ServerID = ServerCount,
	{reply, ServerID, State#masterState{
		games=[{ServerID, ServerPID} | Games], 
		serverCount=ServerCount + 1
	}};

handle_call({register_player, PlayerPID, ServerID}, _From, #masterState{players=Players, playerCount=PlayerCount} = State) ->
	PlayerID = PlayerCount,
	ServerPID = find_game_by_id_(State, ServerID),
	{reply, {PlayerID, ServerPID}, State#masterState{
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
