PATH:=/opt/gnat/bin:${PATH}
libs=-L../../lib -lmachdep
stdout=./central.log
wd=$(shell pwd)

all : dirs
	$(MAKE) -C ../../ all  	
	gprbuild -P./particles.gpr -largs $(libs)

dirs : ./build ./bin $(resultdir)
	$(MAKE) -C../../ dirs

./build ./bin $(resultdir):
	mkdir -p $@

## simulate a central collision
run : $(resultdir) 
	./run.sh --root=$(wd) --config=$(config)

noncentral central:
	$(MAKE) run config=$@

clean : dirs
	gnatclean -r -P./particles.gpr

distclean : clean 
	rm -rf ./build ./bin ./test *.log 
