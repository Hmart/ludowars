-module(ludo_game_player).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, handle_event/3, handle_sync_event/4, 
        handle_info/3, terminate/3, code_change/4, alive/2]).

-include("include/records.hrl").

-record(playerState, {
    id,
    connectionPID,
    gamePID,
    statePID,
    entityID
}).

send_packet(State, Packet) ->
  ludo_game_connection:send_packet(State#playerState.connectionPID, Packet).

start_link(ConnectionPID) ->
    gen_fsm:start_link(?MODULE, ConnectionPID, []).

init(ConnectionPID) ->
    {ID, GamePID} = ludo_master:register_player(0),
    StatePID = ludo_game_server:get_state_server(GamePID),
    ludo_game_state:subscribe(StatePID),
    Entity = ludo_game_state:add_entity(StatePID, #entity{
      controller = "ludowars.controller.PlayerController", %% controller name
      representation = "ludowars.view.PlayerRepresentation", %% representation name
      driver = "ludowars.controller.EntityDriver", %% driver name
      positionX = ID * 48.0 + 128.0, %% X
      positionY = ID * 48.0 + 128.0, %% Y
      velocityX = 0.0, %% velocity X
      velocityY = 0.0, %% velocity Y
      angle = 0.0, %% angle
      width = 16, %% width
      height = 10 %% height
    }),
    EntityID = Entity#entity.id,
    io:format("entity ~p~n", [EntityID]),
    {ok, alive, #playerState{
        id=ID,
        gamePID=GamePID,
        connectionPID=ConnectionPID,
        statePID=StatePID,
        entityID=EntityID
    }}.

alive(_Msg, State) ->
    {next_state, alive, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info({'$gen_cast', {subscribed, GameState}}, StateName, State) ->
  send_packet(State, {state_packet, {GameState}}),
  {next_state, StateName, State};

handle_info({'$gen_cast', {added_entity, Entity}}, StateName, State)
  when Entity#entity.id == State#playerState.entityID ->
  send_packet(State, {add_entity, {Entity}}),
  send_packet(State, {assign_entity_packet, {Entity#entity.id}}),
  {next_state, StateName, State};

handle_info({'$gen_cast', {added_entity, Entity}}, StateName, State) ->
  send_packet(State, {add_entity, {Entity}}),
  {next_state, StateName, State};

handle_info(Info, StateName, State) ->
  io:format("handle_info ~p~n", [Info]),
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
