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
central :: $(resultdir) 
	./run.sh --root=$(wd) --config=central |grep "P " > $(stdout)
	./clean.sh $(stdout) 1
	./clean.sh $(stdout) 2


clean : ./build
	gnatclean -r -P./particles.gpr

distclean : clean 
	rm -rf ./build ./bin ./test *.log 
	$(MAKE) -C../../ distclean
