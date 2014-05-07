-module(ludo_game_connection_sup).
-behaviour(supervisor).

-export([start_link/1, init/1, start_socket/0]).

start_link(GameSupPID) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [GameSupPID]).

init([_GameSupPID]) ->
	{ok, Port} = application:get_env(port),
	io:format("Server started on port: ~p~n", [Port]),
	{ok, ListenSocket} = gen_tcp:listen(Port, [{active, true}, {reuseaddr, true}]),
	spawn_link(fun empty_listeners/0),
	{ok, {{simple_one_for_one, 60, 3600},
		[{socket,
		 {ludo_game_connection, start_link, [ListenSocket]},
		 temporary, 1000, worker, [ludo_game_connection]}
	]}}.

start_socket() ->
	supervisor:start_child(?MODULE, []).

empty_listeners() ->
	[start_socket() || _ <- lists:seq(1, 4)].
