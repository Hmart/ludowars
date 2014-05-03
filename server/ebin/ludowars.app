{application, ludowars, [
	{vsn, "0.1.0"},
	{registered, [ludo]},
	{mod, {ludo_app, []}},
	{env, [{port, 7331}]},
	{modules, [ludo_app, ludo_game_serv, ludo_game_sup, ludo_sup]}
]}.
