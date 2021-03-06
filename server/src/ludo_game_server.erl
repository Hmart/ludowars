%%% @doc Ludo gameserver, This module defines a gameserver.
%%% @end

-module(ludo_game_server).
-behaviour(gen_server).

%% API
-export([start_link/0, get_state_server/1, get_unix_time/0]). %% API.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

-include("include/records.hrl").

-record(serverState, {
	id,
	statePID
}).

%% API.

%% @doc Starts the server.
%%
%% @spec start_link() -> {ok, Pid}
%% where
%%	Pid = pid()
%% @end
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% @doc returns the pid to the stateserver.
%%
%% @spec get_state_server(GamePID::pid()) -> {ok, StatePID}
%% where
%%	StatePID = pid()
%% @end
get_state_server(GamePID) ->
	gen_server:call(GamePID, get_state_pid).

%% @doc Returns the unix epoch time
%%
%% @spec get_unix_time() -> Time::integer()
%% @end
get_unix_time() ->
	{M, S, _} = erlang:now(),
	M * 1000000 + S.

%% gen_server.
init([]) ->
	ServerID = ludo_master:register_game(),
	{ok, StatePID} = ludo_game_state:start_link(),
	gen_server:cast(self(), game_start),
	{ok, #serverState{
		id=ServerID,
		statePID=StatePID
	}}.

handle_call(get_state_pid, _From, State) ->
	{reply, State#serverState.statePID, State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(game_start, State) ->
	Self = self(),
	spawn(fun() -> ludo_game_npc:start_link(Self) end),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
