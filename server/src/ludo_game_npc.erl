-module(ludo_game_npc).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, handle_event/3, handle_sync_event/4, 
        handle_info/3, terminate/3, code_change/4, wander/2, chase/2]).

-include("include/records.hrl").

-record(npcState, {
    gamePID,
    statePID,
    entityID,
    entityPID,
    targetEntityID,
    targetEntityPID,
    timer
}).


start_link(GamePID) ->
    gen_fsm:start_link(?MODULE, GamePID, []).

init(GamePID) ->
    StatePID = ludo_game_server:get_state_server(GamePID),
    ludo_game_state:subscribe(StatePID),
    Entity = ludo_game_state:add_entity(StatePID, #entity{
      controller = "ludowars.controller.PlayerController", %% controller name
      representation = "ludowars.view.PlayerRepresentation", %% representation name
      driver = "ludowars.controller.EntityDriver", %% driver name
      positionX = random:uniform(128) + 512 + 128.0, %% X
      positionY = random:uniform(128) + 512 + 128.0, %% Y
      velocityX = 0.0, %% velocity X
      velocityY = 0.0, %% velocity Y
      angle = 0.0, %% angle
      width = 16, %% width
      height = 10 %% height
    }),
    EntityPID = Entity#entity.pid,
    EntityID = Entity#entity.id,
    io:format("entity ~p~n", [EntityID]),
    Timer = timer:send_interval(21, tick),
    {ok, wander, #npcState{
        gamePID=GamePID,
        statePID=StatePID,
        entityID=EntityID,
        entityPID=EntityPID,
        timer=Timer
    }}.

wander(_Msg, State) ->
  Entity = ludo_game_state:get_entity(State#npcState.statePID, State#npcState.entityID),
  TargetEntity = ludo_game_state:get_closest_entity(State#npcState.statePID, Entity#entity.positionX, Entity#entity.positionY, 150, Entity),
  case TargetEntity of 
    not_found -> {next_state, wander, State};
    _ -> {next_state, chase, State#npcState{
      targetEntityPID=TargetEntity#entity.pid,
      targetEntityID=TargetEntity#entity.id
    }}
  end.

chase(_Msg, State) ->
  Entity = ludo_game_state:get_entity(State#npcState.statePID, State#npcState.entityID),
  TargetEntity = ludo_game_state:get_entity(State#npcState.statePID, State#npcState.targetEntityID),
  case TargetEntity of
    not_found -> {next_state, wander, State};
    _ ->
      Distance = ludo_game_entity:distance(Entity, TargetEntity),
      if 
        Distance > 150 ->
          ludo_game_entity:process_driver_state(State#npcState.entityPID, #driverState{
            entityID=Entity#entity.id,
            positionX=Entity#entity.positionX,
            positionY=Entity#entity.positionY,
            north=0,
            south=0,
            west=0,
            east=0,
            fire=0,
            secondary=0,
            mouseX=TargetEntity#entity.positionX,
            mouseY=TargetEntity#entity.positionY
          }),
          {next_state, wander, State};
        true ->
          ludo_game_entity:process_driver_state(State#npcState.entityPID, #driverState{
            entityID=Entity#entity.id,
            positionX=Entity#entity.positionX,
            positionY=Entity#entity.positionY,
            north=0,
            south=0,
            west=0,
            east=0,
            fire=1,
            secondary=0,
            mouseX=TargetEntity#entity.positionX,
            mouseY=TargetEntity#entity.positionY
          }),
          {next_state, chase, State}
      end
  end.

handle_event(Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info(tick, StateName, State) -> 
  gen_fsm:send_event(self(), tick),
  {next_state, StateName, State};

handle_info(Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
