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
    timer,
    primaryAttackTimer,
    map,
    path
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
      height = 10, %% height
      health = 150.0
    }),
    EntityPID = Entity#entity.pid,
    EntityID = Entity#entity.id,
    io:format("entity ~p~n", [EntityID]),
    Timer = timer:send_interval(21, tick),
    Map = ludo_game_mapparser:main(),
    {ok, wander, #npcState{
        gamePID=GamePID,
        statePID=StatePID,
        entityID=EntityID,
        entityPID=EntityPID,
        timer=Timer,
        primaryAttackTimer=0,
        map=Map
    }}.

wander(_Msg, State) ->
  Entity = ludo_game_state:get_entity(State#npcState.statePID, State#npcState.entityID),
  TargetEntity = ludo_game_state:get_closest_entity(State#npcState.statePID, Entity#entity.positionX, Entity#entity.positionY, 500, Entity),
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
        Distance > 500 ->
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
          Time = ludo_game_server:get_unix_time(),
          {Fire, State2} = if
            State#npcState.primaryAttackTimer =< Time -> 
              {1, State#npcState{primaryAttackTimer=Time + 1}};
            true ->
              {0, State}
          end,
          Path = ludo_game_pathfinding:astar(
              ludo_game_entity:tile_position(Entity), 
              ludo_game_entity:tile_position(TargetEntity),
              State#npcState.map
          ),
          {TileX, TileY} = case Fire of 
            1 ->
              case Path of
                [H|[H2|_T]] -> H2;
                _ -> ludo_game_entity:tile_position(Entity)
              end;
            _ -> ludo_game_entity:tile_position(Entity)
          end,
          CurrentX = Entity#entity.positionX,
          CurrentY = Entity#entity.positionY,
          TargetX = TileX * 32 + 16,
          TargetY = TileY * 32 + 16,

          North = trunc(max(TargetY - CurrentY, 1.0)),
          South = trunc(max(CurrentY - TargetY, 1.0)),
          West = trunc(max(CurrentX - TargetX, 1.0)),
          East = trunc(max(TargetX - CurrentX, 1.0)),

          ludo_game_entity:process_driver_state(State#npcState.entityPID, #driverState{
            entityID=Entity#entity.id,
            positionX=TileX * 32 + 16,
            positionY=TileY * 32 + 16,
            north=1,
            south=0,
            west=0,
            east=0,
            fire=Fire,
            secondary=0,
            mouseX=TargetEntity#entity.positionX,
            mouseY=TargetEntity#entity.positionY
          }),
          {next_state, chase, State2}
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
