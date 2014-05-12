-module(ludo_game_state).
-behaviour(gen_server).

-export([start_link/0, add_entity/2, delete_entity/2, update_entity/2, 
		find_entity_by_id/2, get_state/1, subscribe/1, unsubscribe/1]). %% API.
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

find_entity_by_id(StatePID, EntityID) ->
	gen_server:call(StatePID, {find_entity_by_id, EntityID}).

get_state(StatePID) ->
	gen_server:call(StatePID, get_state).

subscribe(StatePID) ->
	gen_server:call(StatePID, {subscribe, self()}).

unsubscribe(_StatePID) ->
	%% TODO: call this automatically for dying processes (use monitors)
	tbi.

%% Private API.
get_free_entity_id(#state{entities=[]}) ->
	0;

get_free_entity_id(#state{entities=[H|_T]}) ->
	H#entity.id + 1.

send(Subscriber, Message) ->
	gen_server:cast(Subscriber, Message).

broadcast(#state{subscribers=Subscribers}, Message) ->
	lists:map(fun (S) -> send(S, Message) end, Subscribers).

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
	NewEntity = Entity#entity{id=ID},
	NewState = State#state{
		entities=[NewEntity|State#state.entities], 
		entityCount=State#state.entityCount + 1
	},
	broadcast(NewState, {added_entity, NewEntity}),
	{reply, NewEntity, NewState};

handle_call({delete_entity, EntityID}, _From, State) ->
	NewEntities = [E || E <- State#state.entities, E#entity.id =/= EntityID],
	NewState = State#state{
		entities=NewEntities,
		entityCount=State#state.entityCount - 1
	},
	broadcast(NewState, {deleted_entity, EntityID}),
	{reply, ok, NewState};

handle_call({update_entity, Entity}, _From, State) ->
	NewEntities = lists:keyreplace(Entity#entity.id, 2, State#state.entities, Entity),
	NewState = State#state{
		entities=NewEntities
	},
	broadcast(NewState, {updated_entity, Entity}),
	{reply, ok, NewState};

handle_call({find_entity_by_id, EntityID}, _From, State) ->
	L = [Entity || Entity = #entity{id=ID} <- State#state.entities, ID == EntityID],
	FoundEntity = case L of
		[] -> not_found;
		[Entity] -> Entity
	end,
	{reply, FoundEntity, State};

handle_call(get_state, _From, State) ->
	{reply, State, State};

handle_call({subscribe, PID}, _From, State) ->
	NewState = State#state{
		subscribers=[PID|State#state.subscribers]
	},
	{reply, ok, NewState};

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