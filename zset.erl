-module(zset).
-export([
	load/0,
	create/0,
	insert/3,
	delete/3,
	delete_by_rank/3,
	get_count/1,
	get_rank/3,
	get_rank_range/3,
	get_score_range/3
]).

load()->
    zset_nif:load().

create()->
    zset_nif:create().
	
insert(Z,Score,Key)when is_number(Score), is_list(Key)->
    zset_nif:insert(Z, float(Score), Key).

delete(Z,Score,Key)when is_number(Score), is_list(Key)->
    zset_nif:delete(Z, float(Score), Key).
	
delete_by_rank(Z,Start,End) when is_integer(Start), is_integer(End)->
    zset_nif:delete_by_rank(Z, Start, End).

get_count(Z)->
    zset_nif:get_count(Z).

get_rank(Z,Score,Key)when is_number(Score), is_list(Key)->
    zset_nif:get_rank(Z, float(Score), Key).

get_rank_range(Z,R1,R2)when is_integer(R1), is_integer(R2)->
    zset_nif:get_rank_range(Z, R1, R2).

get_score_range(Z,S1,S2)when is_number(S1), is_number(S2)->
    zset_nif:get_score_range(Z, float(S1), float(S2)).




