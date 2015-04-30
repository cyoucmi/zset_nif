-module(zset_nif).
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
	erlang:load_nif("./zset_nif", 0).

create()->
	"zset_nif library not loaded".

	
insert(_,_,_)->
	"zset_nif library not loaded".

delete(_,_,_)->
	"zset_nif library not loaded".
	
delete_by_rank(_,_,_)->
	"zset_nif library not loaded".

get_count(_)->
	"zset_nif library not loaded".	

get_rank(_,_,_)->
	"zset_nif library not loaded".

get_rank_range(_,_,_)->
	"zset_nif library not loaded".

get_score_range(_,_,_)->
	"zset_nif library not loaded".	

