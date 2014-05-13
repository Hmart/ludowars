-module(ludo_game_server).
-behaviour(gen_server).

-export([start_link/0, get_state_server/1]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

-include("include/records.hrl").

-record(serverState, {
	id,
	statePID
}).

%% API.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

get_state_server(GamePID) ->
	gen_server:call(GamePID, get_state_pid).

%% gen_server.
init([]) ->
	ServerID = ludo_master:register_game(),
	StatePID = ludo_game_state:start_link(),
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

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.