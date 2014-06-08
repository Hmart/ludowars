-module(ludo_game_state).
-behaviour(gen_server).

-export([start_link/0, add_entity/2, delete_entity/2, update_entity/2, 
		get_entity/2,get_entities_in_range/4, get_closest_entity/4, get_closest_player/5, get_state/1, subscribe/1, subscribe/2,
		unsubscribe/1, unsubscribe/2, update_driver_state/2, distance/4, get_closest_entity/5,
		update_entity_health/2]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

-include("include/records.hrl").

%% Public API.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

add_entity(StatePID, Entity) ->
	gen_server:call(StatePID, {add_entity, Entity}).

delete_entity(StatePID, EntityID) ->
	gen_server:call(StatePID, {delete_entity, EntityID}).

update_entity(StatePID, Entity) ->
	gen_server:call(StatePID, {update_entity, Entity}).

update_driver_state(StatePID, DriverState) ->
	gen_server:call(StatePID, {update_driver_state, DriverState}).

update_entity_health(StatePID, Entity) ->
	update_entity(StatePID, Entity),
	gen_server:call(StatePID, {update_entity_health, Entity}).

get_entity(StatePID, EntityID) ->
	gen_server:call(StatePID, {find_entity_by_id, EntityID}).

get_entities_in_range(StatePID, X, Y, R) ->
	gen_server:call(StatePID, {find_entities_in_range, X, Y, R}).

get_closest_entity(StatePID, X, Y, R, E) ->
	gen_server:call(StatePID, {get_closest_entity, X, Y, R, E}).

get_closest_player(StatePID, X, Y, R, E) ->
	gen_server:call(StatePID, {get_closest_entity, X, Y, R, E}).

get_closest_entity(StatePID, X, Y, R) ->
	gen_server:call(StatePID, {get_closest_entity, X, Y, R, undefined}).

get_state(StatePID) ->
	gen_server:call(StatePID, get_state).

subscribe(StatePID, SubscriberPID) ->
	gen_server:call(StatePID, {subscribe, SubscriberPID}).

subscribe(StatePID) ->
	subscribe(StatePID, self()).

unsubscribe(StatePID, SubscriberPID) ->
	gen_server:cast(StatePID, {unsubscribe, SubscriberPID}).

unsubscribe(StatePID) ->
	unsubscribe(StatePID, self()).

%% Private API.
get_free_entity_id(#state{entities=[]}) ->
	0;

get_free_entity_id(#state{entities=[H|_T]}) ->
	H#entity.id + 1.

distance(X1, Y1, X2, Y2) ->
	Xd = X2 - X1,
	Yd = Y2 - Y1,
	math:sqrt((Xd * Xd) + (Yd * Yd)).

notify(#state{subscribers=Subscribers}, Message) ->
	lists:map(fun (S) -> notify(S, Message) end, Subscribers);

notify(SubscriberPID, Message) ->
	gen_server:cast(SubscriberPID, Message).

%% gen_server.
init([]) ->
	State = #state{
		worldBoundsX = 0.0, %% world boundaries: x
		worldBoundsY = 0.0, %% world boundaries: y
		worldBoundsWidth = 1280.0, %% world boundaries: width
		worldBoundsHeight = 1024.0, %% world boundaries: height
		entities = [],
		entityCount = 0,
		subscribers = []
	},
	{ok, State}.

handle_call({add_entity, Entity}, _From, State) ->
	ID = get_free_entity_id(State),
	Entity2 = Entity#entity{id=ID, statePID=self()},
	{ok, EntityPID} = ludo_game_entity:start_link(Entity2),
	Entity3 = Entity2#entity{pid=EntityPID},
	NewState = State#state{
		entities=[Entity3|State#state.entities], 
		entityCount=State#state.entityCount + 1
	},
	notify(NewState, {added_entity, Entity3}),
	{reply, Entity3, NewState};

handle_call({delete_entity, EntityID}, _From, State) ->
	NewEntities = [E || E <- State#state.entities, E#entity.id =/= EntityID],
	NewState = State#state{
		entities=NewEntities,
		entityCount=State#state.entityCount - 1
	},
	notify(NewState, {deleted_entity, EntityID}),
	{reply, ok, NewState};

handle_call({update_entity, Entity}, _From, State) ->
	NewEntities = lists:keyreplace(Entity#entity.id, 2, State#state.entities, Entity),
	NewState = State#state{
		entities=NewEntities
	},
	notify(NewState, {updated_entity, Entity}),
	{reply, ok, NewState};

handle_call({update_driver_state, DriverState}, _From, State) ->
	notify(State, {updated_driver_state, DriverState}),
	{reply, ok, State};

handle_call({update_entity_health, Entity}, _From, State) ->
  	io:format("ludo_game_state:handle_call update_entity_health ~p~n", [Entity]),
	notify(State, {health_updated, Entity#entity.id, Entity#entity.health}),
	{reply, ok, State};

handle_call({find_entity_by_id, EntityID}, _From, State) ->
	L = [Entity || Entity = #entity{id=ID} <- State#state.entities, ID == EntityID],
	FoundEntity = case L of
		[] -> not_found;
		[Entity] -> Entity
	end,
	{reply, FoundEntity, State};

handle_call({get_entities_in_range, _X, _Y, _R}, _From, _State) ->
	tbi;

handle_call({get_closest_entity, X, Y, R, E}, _From, State) ->
	L = [Entity || Entity = #entity{id=ID, positionX=PositionX, positionY=PositionY} <- State#state.entities, 
		R >= distance(X, Y, PositionX, PositionY), E#entity.id =/= ID],
	FoundEntity = case L of
		[] -> not_found;
		[Entity|_T] -> Entity
	end,
	{reply, FoundEntity, State};

handle_call({get_closest_player, X, Y, R, E}, _From, State) ->
	Players = ludo_master:players_by_server_id(0),
	L = [Entity || Entity = #entity{id=ID, positionX=PositionX, positionY=PositionY} <- State#state.entities, 
	R >= distance(X, Y, PositionX, PositionY), E#entity.id =/= ID],
	FoundEntity = case L of
		[] -> not_found;
		[Entity|_T] -> Entity
	end,



	{reply, FoundEntity, State};

handle_call(get_state, _From, State) ->
	{reply, State, State};

handle_call({subscribe, PID}, _From, State) ->
	NewState = State#state{
		subscribers=[PID|State#state.subscribers]
	},
	monitor(process, PID),
	notify(PID, {subscribed, NewState}),
	{reply, ok, NewState};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({unsubscribe, PID}, State) ->
	NewState = State#state{
		subscribers=lists:delete(PID, State#state.subscribers)
	},
	{noreply, NewState};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'DOWN', _MonitorRef, _Type, SubscriberPID, _Info}, State) ->
	unsubscribe(SubscriberPID),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.