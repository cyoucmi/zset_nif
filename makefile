FLAGS = -g 
##-O6
##-std=c99

## TODO:
ERL_ROOT = /usr/local/lib/erlang/erts-5.10.4

ECC = erlc

OUTDIR = /root/zset
RCS = $(wildcard *.erl)
OBJS = $(patsubst %.erl,$(OUTDIR)/%.beam,$(RCS))

all:$(OUTDIR)/zset_nif.so $(OUTDIR)/test $(OBJS)

$(OUTDIR)/zset_nif.so:t_zset.c t_zset.h zset_nif.c
	gcc -o $@ $^ --shared -fpic $(FLAGS) -Wall -I $(ERL_ROOT)/emulator/beam -I $(ERL_ROOT)/include
	
$(OUTDIR)/test:t_zset.c t_zset.h test.c
	gcc -o $@ $^ -Wall 
	
$(OUTDIR)/%.beam:%.erl
	$(ECC) -o $(OUTDIR) $^

clean: 
	rm  $(OUTDIR)/zset_nif.so $(OUTDIR)/*.beam $(OUTDIR)/test
