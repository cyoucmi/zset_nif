#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "t_zset.h"
#include "erl_nif.h"

#define INIT_ARRAY_SIZE 1


typedef struct ZsetNif{
    zskiplist *zset;
	ErlNifEnv *env;
	ERL_NIF_TERM *array;
	unsigned long size;
	unsigned long used_size;
}ZsetNif;

static ErlNifResourceType *zset_resource_type;

static void
zset_resource_destory(ErlNifEnv *env, void *obj){
    struct ZsetNif *zset_nif = obj;
	enif_free(zset_nif->array);
    zslFree(zset_nif->zset);
}

static int
zset_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info){
    ErlNifResourceFlags tried;
    zset_resource_type = enif_open_resource_type(env, NULL, "zset_resource_type", zset_resource_destory, ERL_NIF_RT_CREATE,&tried);
	return 0;
};


static void 
zset_unload(ErlNifEnv *env, void *priv_data){
}


static ERL_NIF_TERM 
zset_create_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
	
    ZsetNif *zset_nif = enif_alloc_resource(zset_resource_type,sizeof(ZsetNif));
    zset_nif->zset = zslCreate();
	zset_nif->array = enif_alloc(sizeof(ERL_NIF_TERM) * INIT_ARRAY_SIZE);
	zset_nif->size = INIT_ARRAY_SIZE;
	zset_nif->used_size = 0;

    ERL_NIF_TERM result = enif_make_resource(env, zset_nif);
    enif_release_resource(zset_nif);

    return result;
}


static ERL_NIF_TERM 
zset_insert_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){

    ZsetNif *zset_nif;
    if(!enif_get_resource(env, argv[0], zset_resource_type, (void**)&zset_nif)){
         return enif_make_atom(env, "in zset_insert_nif, arg[0] is not a handle to a resource object");
    }
	
    double score;
    enif_get_double(env, argv[1], &score);	
	
	unsigned bufflen = 128, size;
	char buf[bufflen];
	
	if((size = enif_get_string(env, argv[2], buf, bufflen, ERL_NIF_LATIN1)) <= 0){
        return enif_make_atom(env, "in zset_insert_nif, arg[2] something wrong");	
	};
	
	zslInsert(zset_nif->zset, score, create_robj(buf, size));
	
    return enif_make_atom(env, "ok");	
}

static ERL_NIF_TERM 
zset_delete_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){

    ZsetNif *zset_nif;
    if(!enif_get_resource(env, argv[0], zset_resource_type, (void**)&zset_nif)){
         return enif_make_atom(env, "in zset_delete_nif, arg[0] is not a handle to a resource object");
    }
	
    double score;
    enif_get_double(env, argv[1], &score);	
	
	unsigned bufflen = 128, size;
	char buf[bufflen];
	
	if((size = enif_get_string(env, argv[2], buf, bufflen, ERL_NIF_LATIN1)) <= 0){
        return enif_make_atom(env, "in zset_delete_nif, arg[2] something wrong");	
	};
	
	robj  obj;
	obj.data = buf;
	obj.len = size;

	int result = zslDelete(zset_nif->zset, score, &obj);
	if(result)
		return enif_make_atom(env, "true");
	else
		return enif_make_atom(env, "false");
}

static void
delete_cb(void *ud, zskiplistNode *node){
	ZsetNif *zset_nif = (ZsetNif *) ud;
	if(zset_nif->used_size >= zset_nif->size){
		unsigned long size = zset_nif->size;
		unsigned long new_size = size * 2;
		ERL_NIF_TERM *new_array = enif_alloc(sizeof(ERL_NIF_TERM) * new_size);
		memcpy(new_array, zset_nif->array, sizeof(ERL_NIF_TERM) * size);
        enif_free(zset_nif->array);
		zset_nif->array = new_array;
		zset_nif->size = new_size;
	}
	if(node->obj)
		zset_nif->array[zset_nif->used_size++] = enif_make_string_len(zset_nif->env, node->obj->data, node->obj->len-1, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM 
zset_delete_by_rank_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){

    ZsetNif *zset_nif;
    if(!enif_get_resource(env, argv[0], zset_resource_type, (void**)&zset_nif)){
         return enif_make_atom(env, "in zset_delete_by_rank_nif, arg[0] is not a handle to a resource object");
    }
	
	unsigned int start, end;
	enif_get_uint(env, argv[1], &start);
	enif_get_uint(env, argv[2], &end);	

    if (start > end) {
        unsigned int tmp = start;
        start = end;
        end = tmp;
    }
	zset_nif->env = env;
	zset_nif->used_size = 0;
	zslDeleteByRank(zset_nif->zset, start, end, delete_cb, (void *)zset_nif);
	return enif_make_list_from_array(env, zset_nif->array, zset_nif->used_size);
}

static ERL_NIF_TERM 
zset_get_count_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){

    ZsetNif *zset_nif;
    if(!enif_get_resource(env, argv[0], zset_resource_type, (void**)&zset_nif)){
         return enif_make_atom(env, "in zset_get_count_nif, arg[0] is not a handle to a resource object");
    }

	return enif_make_ulong(env, zset_nif->zset->length);
}

static ERL_NIF_TERM 
zset_get_rank_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){

    ZsetNif *zset_nif;
    if(!enif_get_resource(env, argv[0], zset_resource_type, (void**)&zset_nif)){
         return enif_make_atom(env, "in zset_get_rank_nif, arg[0] is not a handle to a resource object");
    }
	
    double score;
    enif_get_double(env, argv[1], &score);	
	
	unsigned bufflen = 128, size;
	char buf[bufflen];
	
	if((size = enif_get_string(env, argv[2], buf, bufflen, ERL_NIF_LATIN1)) <= 0){
        return enif_make_atom(env, "in zset_get_rank_nif, arg[2] something wrong");	
	};
	
	robj  obj;
	obj.data = buf;
	obj.len = size;
	
	unsigned long rank = zslGetRank(zset_nif->zset, score, &obj);	
	
	return enif_make_ulong(env, rank);
}


static ERL_NIF_TERM 
zset_get_rank_range_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){

    ZsetNif *zset_nif;
    if(!enif_get_resource(env, argv[0], zset_resource_type, (void**)&zset_nif)){
         return enif_make_atom(env, "in zset_get_rank_range_nif, arg[0] is not a handle to a resource object");
    }
	
	unsigned long r1, r2;
	enif_get_ulong(env, argv[1], &r1);
	enif_get_ulong(env, argv[2], &r2);	
	
    int reverse, rangelen;
    if(r1 <= r2) {
        reverse = 0;
        rangelen = r2 - r1 + 1;
    } else {
        reverse = 1;
        rangelen = r1 - r2 + 1;
    }

    zskiplistNode* node = zslGetElementByRank(zset_nif->zset, r1);
	ERL_NIF_TERM array[rangelen];
    int n = 0;
    while(node && n < rangelen) {
		if(node->obj)
			array[n++] = enif_make_string_len(env, node->obj->data, node->obj->len-1, ERL_NIF_LATIN1);
        node = reverse? node->backward : node->level[0].forward;
	} 
	return enif_make_list_from_array(env, array, n);
}

static ERL_NIF_TERM 
zset_get_score_range_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){

    ZsetNif *zset_nif;
    if(!enif_get_resource(env, argv[0], zset_resource_type, (void**)&zset_nif)){
         return enif_make_atom(env, "in zset_get_score_range_nif, arg[0] is not a handle to a resource object");
    }
	
	double s1, s2;
	enif_get_double(env, argv[1], &s1);
	enif_get_double(env, argv[2], &s2);	
	
	int reverse; 
    zskiplistNode *node, *node_raw;
	
	zrangespec range;
	range.minex = 1;
	range.maxex = 1;

    if(s1 <= s2) {
        reverse = 0;
		range.min = s1;
		range.max = s2;
        node_raw = zslFirstInRange(zset_nif->zset, &range);
    } else {
        reverse = 1;
		range.min = s2;
		range.max = s1;		
        node_raw = zslLastInRange(zset_nif->zset, &range);
    }
	node = node_raw;
    int n = 0;
    while(node) {
        if(reverse) {
            if(node->score < s2) break;
        } else {
            if(node->score > s2) break;
        }
		if(node->obj) n++;
        node = reverse? node->backward:node->level[0].forward;
    }
	ERL_NIF_TERM array[n];
    n = 0;
	node = node_raw;	
    while(node) {
        if(reverse) {
            if(node->score < s2) break;
        } else {
            if(node->score > s2) break;
        }
		if(node->obj)
			array[n++] = enif_make_string_len(env, node->obj->data, node->obj->len-1, ERL_NIF_LATIN1);
        node = reverse? node->backward:node->level[0].forward;
    }	
	return enif_make_list_from_array(env, array, n);
}


static ErlNifFunc nif_funcs[] = {
	{"create", 0, zset_create_nif},	
	{"insert", 3, zset_insert_nif},
	{"delete", 3, zset_delete_nif},
    {"delete_by_rank", 3, zset_delete_by_rank_nif},
    {"get_count", 1, zset_get_count_nif},
    {"get_rank", 3, zset_get_rank_nif},
    {"get_rank_range", 3, zset_get_rank_range_nif},
    {"get_score_range", 3, zset_get_score_range_nif},	
};

ERL_NIF_INIT(zset_nif, nif_funcs, zset_load, NULL, NULL, zset_unload);
