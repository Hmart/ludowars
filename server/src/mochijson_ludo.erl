-module(test_mochijson2).
-export([readlines/1, get_all_lines/2]).

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).

get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), string:join(Accum, "");
        Line -> get_all_lines(Device, Accum ++ [Line])
    end.

% mochijson2:decode(test_mochijson2:readlines("../../client/assets/maps/forest.json")).
% http://www.erlang.org/doc/man/lists.html#keysearch-3
% client/assets/maps/forest.json