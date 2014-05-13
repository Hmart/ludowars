-module(ludo_game_player).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, handle_event/3, handle_sync_event/4, 
        handle_info/3, terminate/3, code_change/4, alive/2]).

-record(playerState, {
    id,
    connectionPID,
    gamePID,
    statePID,
    entityID
}).

start_link(ConnectionPID) ->
    gen_fsm:start_link(?MODULE, ConnectionPID, []).

init(ConnectionPID) ->
    {ID, GamePID} = ludo_master:register_player(0),
    {ok, alive, #playerState{
        id=ID,
        gamePID=GamePID,
        connectionPID=ConnectionPID
    }}.

alive(_Msg, State) ->
    {next_state, alive, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
