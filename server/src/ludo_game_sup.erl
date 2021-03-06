-module(ludo_game_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{one_for_one, 60, 3600},
		[{ludo_game_connection_sup,
		 {ludo_game_connection_sup, start_link, [self()]},
		 transient, infinity, supervisor, [ludo_game_connection_sup]},
		 {ludo_game_server,
		 {ludo_game_server, start_link, []},
		 transient, infinity, worker, [ludo_game_server]}
	]}}.
