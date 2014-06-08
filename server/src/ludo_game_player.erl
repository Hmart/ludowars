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
    entityID,
    entityPID
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
      height = 10, %% height
      health = 150.0
    }),
    EntityPID = Entity#entity.pid,
    EntityID = Entity#entity.id,
    io:format("entity ~p~n", [EntityID]),
    {ok, alive, #playerState{
        id=ID,
        gamePID=GamePID,
        connectionPID=ConnectionPID,
        statePID=StatePID,
        entityID=EntityID,
        entityPID=EntityPID
    }}.

alive(_Msg, State) ->
    {next_state, alive, State}.

handle_packet({chat_packet, {Text}}, StateName, State) ->
  FormattedText = string:concat(io_lib:format("Player ~p: ", [State#playerState.id]), Text),
  ludo_master:broadcast(0, {chat_packet, {FormattedText}}),
  io:format("chat_packet ~p~n", [{chat_packet, {FormattedText}}]),
  {next_state, StateName, State};

handle_packet({damage, {_Source, Target, Damage}}, StateName, State) ->
  TargetEntity = ludo_game_state:get_entity(State#playerState.statePID, Target),
  io:format("ludo_game_player:handle_packet damage ~p~n", [TargetEntity]),
  case TargetEntity of
    not_found -> ok;
    #entity{pid=EntityPID} -> ludo_game_entity:change_health(EntityPID, Damage)
  end,
  {next_state, StateName, State};

handle_packet({move_packet, DriverState}, StateName, State) ->
  ludo_game_entity:process_driver_state(State#playerState.entityPID, DriverState),
  {next_state, StateName, State};

handle_packet(_Packet, StateName, State) ->
  {next_state, StateName, State}.

handle_event(P = {packet, Packet}, StateName, State) ->
  handle_packet(Packet, StateName, State);

handle_event(disconnect, StateName, State) ->
  io:format("PLAYER DISCONNECT ~p, ~p~n", [State#playerState.entityID, State#playerState.entityPID]),
  ludo_game_entity:delete_entity(State#playerState.entityPID),
  {next_state, StateName, State};

handle_event(Event, StateName, State) ->
  io:format("handle_event ~p~n", [Event]),
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info({'$gen_cast', {chat_packet, {Msg}}}, StateName, State) ->
   io:format("gen_cast chat ~p~n", [{chat, Msg}]),

  send_packet(State, {chat_packet, {Msg}}),
  {next_state, StateName, State};

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

handle_info({'$gen_cast', {deleted_entity, EntityID}}, StateName, State) ->
  send_packet(State, {delete_entity, {EntityID}}),
  {next_state, StateName, State};

handle_info({'$gen_cast', {updated_driver_state, DriverState}}, StateName, State)
  when DriverState#driverState.entityID =/= State#playerState.entityID ->
  %io:format("driver state for ~p ~p~n", [DriverState#driverState.entityID, DriverState]),
  send_packet(State, {move_packet, DriverState}),
  {next_state, StateName, State};

handle_info({'$gen_cast', {health_updated, EntityID, EntityHealth}}, StateName, State) ->
  %io:format("driver state for ~p ~p~n", [DriverState#driverState.entityID, DriverState]),
  send_packet(State, {update_health, EntityID, EntityHealth}),
  {next_state, StateName, State};

handle_info(_Info, StateName, State) ->
  %%io:format("handle_info ~p~n", [Info]),
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
