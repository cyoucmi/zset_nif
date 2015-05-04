-module(test).
-compile([export_all]).

test()->
    zset:load(),
    Z = zset:create(),
    zset:insert(Z, 3, "3"),
    zset:insert(Z, 1, "1"),
    zset:insert(Z, 2, "2"),
    zset:get_count(Z),
    io:format("~w", [zset:get_rank_range(Z, 0, 2)]),
    zset:delete_by_rank(Z, 1, 2),
    zset:get_count(Z).
