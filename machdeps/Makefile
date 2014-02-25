rundir=..

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

scheduling.o : scheduling.ads
	gnatmake -c scheduling.ads

$(lib) : set_scheduling.o scheduling.o
	ar crvf $(lib)	set_scheduling.o
	ranlib $(lib)
	cp *.ads *.ali $(libdir)


clean ::
	rm -rf *.o $(lib)

distclean ::  clean
	rm -rf *.ali
