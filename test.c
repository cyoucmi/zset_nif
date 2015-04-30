#include "t_zset.h"

void main(){
    zskiplist *z = zslCreate();
    zslInsert(z, 1, create_robj("1", 1)); 
    zslInsert(z, 2, create_robj("2", 2));
    zskiplistNode* node = zslGetElementByRank(z, 1); 
    zslFree(z);

}
