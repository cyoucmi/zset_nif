#ifndef __T_ZSET_H__
#define __T_ZSET_H__

typedef struct robj{
    void *data;
    unsigned long len;
} robj;

/* Struct to hold a inclusive/exclusive range spec by score comparison. */
typedef struct {
    double min, max;
    int minex, maxex; /* are min or max exclusive? */
} zrangespec;

/* ZSETs use a specialized version of Skiplists */
typedef struct zskiplistNode {
    robj *obj;
    double score;
    struct zskiplistNode *backward;
    struct zskiplistLevel {
        struct zskiplistNode *forward;
        unsigned int span;
    } level[];
}zskiplistNode;

typedef struct zskiplist {
    struct zskiplistNode *header, *tail;
    unsigned long length;
    int level;
}zskiplist;

typedef void (*DelCb)(void *ud, zskiplistNode *node);

robj *create_robj(void *data, unsigned long len);
void destory_robj(robj *obj);


zskiplist *zslCreate(void);
void zslFree(zskiplist *zsl);
zskiplistNode *zslInsert(zskiplist *zsl, double score, robj *obj);
int zslDelete(zskiplist *zsl, double score, robj *obj);
unsigned long zslDeleteByRank(zskiplist *sl, unsigned int start, unsigned int end, DelCb cb, void *ud);
zskiplistNode *zslFirstInRange(zskiplist *zsl, zrangespec *range);
zskiplistNode *zslLastInRange(zskiplist *zsl, zrangespec *range);
unsigned long zslGetRank(zskiplist *zsl, double score, robj *o);
zskiplistNode* zslGetElementByRank(zskiplist *zsl, unsigned long rank);

#endif // __T_ZSET_H__
