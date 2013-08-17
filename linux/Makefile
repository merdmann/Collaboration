rundir=../

## internal stuff
arch=$(shell uname -p)
host=$(shell hostname)
date=$(shell date +%s)

libdir=$(rundir)/lib
lib=$(libdir)/libmachdep.a

%.o :  %.c
	gcc $(CFLAGS) -D_GNU_SOURCE -c $? -o $@

all :: $(libdir) $(lib)

$(libdir) :
	$(MAKE) -C ../ dirs

$(lib) : set_scheduling.o
	ar crvf $(lib)	set_scheduling.o
	ranlib $(lib)

clean ::
	rm -rf *.o $(lib)

distclean ::  clean

