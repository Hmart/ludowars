%%% @doc game entity, This module defines an Entity.
%%% @end

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
	process_driver_state/2,
	distance/2,
	tile_position/1,
	change_health/2
]).

%% gen_Server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

-include("include/records.hrl").

%% Public API.

%% @doc Starts the server.
%%
%% @spec start_link(Entity) -> {ok, Pid}
%% where
%%	Pid = pid()
%% @end
start_link(Entity) ->
	gen_server:start_link(?MODULE, [Entity], []).

%% @doc Returns the entity connected to the EntityPID.
%%
%% @spec get_entity(EntityPID::pid()) -> {ok, Pid}
%% where
%%	Pid = pid()
%% @end
get_entity(EntityPID) ->
	gen_server:call(EntityPID, get_state).

%% @doc Deletes an entity.
%%
%% @spec delete_entity(EntityPID::pid()) -> ok
%% @end
delete_entity(EntityPID) ->
	gen_server:call(EntityPID, delete_entity).

%% @doc Updates an entity.
%%
%% @spec update_entity(EntityPID::pid(), Entity) -> ok
%% @end
update_entity(EntityPID, Entity) ->
	gen_server:call(EntityPID, {update_entity, Entity}).

%% @doc Changes the health of an entity.
%%
%% @spec change_health(EntityPID::pid(), Health::float()) -> ok
%% @end
change_health(EntityPID, Health) ->
	Entity = get_entity(EntityPID),
	Entity2 = Entity#entity{
		health=Entity#entity.health + Health
	},
	gen_server:call(EntityPID, {update_entity_health, Entity2}),
	Entity2.

%% @doc Returns the id connected to an entity.
%%
%% @spec get_entity_id(EntityPID::pid()) -> EntityID::integer()
%% @end
get_entity_id(EntityPID) ->
	Entity = get_entity(EntityPID),
	Entity#entity.id.

%% @doc Sets the position of an entity.
%%
%% @spec set_position(EntityPID::pid(), PosX::interger(), PosY::integer()) -> ok
%% @end
set_position(EntityPID, PosX, PosY) ->
	Entity = get_entity(EntityPID),
	Entity2 = Entity#entity{
		positionX=PosX,
		positionY=PosY
	},
	update_entity(EntityPID, Entity2),
	Entity2.

%% @doc Updates an entity with a driverstate.
%%
%% @spec process_driver_state(EntityPID::pid(), DriverState) -> ok
%% @end
process_driver_state(EntityPID, DriverState) ->
	#driverState{
		positionX=X,
		positionY=Y
	} = DriverState,
	set_position(EntityPID, X, Y),
	gen_server:call(EntityPID, {update_driver_state, DriverState}).

%% @doc Returns the distance between two entities.
%%
%% @spec distance(Entity1, Entity2) -> Distance::float()
%% @end
distance(Entity1, Entity2) ->
	ludo_game_state:distance(
		Entity1#entity.positionX,
		Entity1#entity.positionY,
		Entity2#entity.positionX,
		Entity2#entity.positionY
	).

%% @doc Returns the tileposition of an entity.
%%
%% @spec tile_position(Entity) -> {TileX::integer(), TileY::integer()}
%% @end
tile_position(Entity) ->
	{round(Entity#entity.positionX) div 32, round(Entity#entity.positionY) div 32}.

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

handle_call({update_entity_health, UpdatedEntity}, _From, Entity) ->
	ludo_game_state:update_entity_health(Entity#entity.statePID, UpdatedEntity),
	{reply, ok, UpdatedEntity};

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
