PATH:=/opt/gnat/bin:${PATH}

stdout=./central.log
wd=$(shell pwd)
config=central

all : dirs
	$(MAKE) -C ../../ all  	
	gnatmake -P./schroedinger.gpr 

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
	-gnatclean -r -P./schroedinger.gpr

distclean : clean 
	rm -rf ./build ./bin ./test *.log 
