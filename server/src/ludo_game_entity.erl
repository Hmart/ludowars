-module(ludo_game_entity).
-behaviour(gen_server).

%% API.
-export([
	start_link/1, 
	delete_entity/1, 
	update_entity/2,
	get_entity/1,
	get_entity_id/1,
	set_position/3,
	process_driver_state/2
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

-include("include/records.hrl").

%% Public API.
start_link(Entity) ->
	gen_server:start_link(?MODULE, [Entity], []).

get_entity(EntityPID) ->
	gen_server:call(EntityPID, get_state).

delete_entity(EntityPID) ->
	gen_server:call(EntityPID, delete_entity).

update_entity(EntityPID, Entity) ->
	gen_server:call(EntityPID, {update_entity, Entity}).

get_entity_id(EntityPID) ->
	Entity = get_entity(EntityPID),
	Entity#entity.id.

set_position(EntityPID, PosX, PosY) ->
	Entity = get_entity(EntityPID),
	Entity2 = Entity#entity{
		positionX=PosX,
		positionY=PosY
	},
	update_entity(EntityPID, Entity2),
	Entity2.

process_driver_state(EntityPID, DriverState) ->
	#driverState{
		positionX=X,
		positionY=Y
	} = DriverState,
	set_position(EntityPID, X, Y),
	gen_server:call(EntityPID, {update_driver_state, DriverState}).

%% gen_server.
init([Entity]) ->
	Entity2 = Entity#entity{pid=self()},
	{ok, Entity2}.

stop() ->
	gen_server:cast(self(), stop).

handle_call(get_state, _From, Entity) ->
	{reply, Entity, Entity};

handle_call(delete_entity, _From, Entity) ->
	stop(),
	{reply, ok, Entity};

handle_call({update_entity, UpdatedEntity}, _From, Entity) ->
	ludo_game_state:update_entity(Entity#entity.statePID, UpdatedEntity),
	{reply, ok, UpdatedEntity};

handle_call({update_driver_state, DriverState}, _From, Entity) ->
	ludo_game_state:update_driver_state(Entity#entity.statePID, DriverState),
	{reply, ok, Entity};

handle_call(Request, _From, Entity) ->
	io:format("entity handle_call: ~p~n", [Request]),
	{reply, ignored, Entity}.

handle_cast(stop, Entity) ->
  	io:format("DELETE ENTITY ~p, ~p~n", [Entity#entity.statePID, Entity#entity.id]),
	ludo_game_state:delete_entity(Entity#entity.statePID, Entity#entity.id),
	{stop, normal, Entity};

handle_cast(_Msg, Entity) ->
	{noreply, Entity}.

handle_info(_Info, Entity) ->
	{noreply, Entity}.

terminate(_Reason, _Entity) ->
	ok.

code_change(_OldVsn, Entity, _Extra) ->
	{ok, Entity}.