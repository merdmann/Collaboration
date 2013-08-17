PATH:=/opt/gnat/bin:${PATH}

tmp=/tmp
dist=collaboration-1.0.1
home=$(shell pwd)
os=$(shell uname)
ws=$(tmp)/$(dist)

## default target
all : dirs machdeps library 

## work directories
dirs : ./lib ./build

./lib ./build :
	mkdir -p $@

## build the operating system dep parts
machdeps : $(os)

Linux:
	$(MAKE) -C./linux

## the actual library
library : 
	gprbuild -P./collaboration.gpr


## build distribution
$(ws):
	rm -rf $(ws)
	mkdir -p $(ws)

dist : disclean $(ws)
	cp -a * $(ws)
	( cd $(ws)/../ && tar cvf $(home)/$(dist)-src.tar --exclude-vcs $(dist) )
	rm -rf $(ws)
	gzip $(dist)-src.tar

## cleanup
clean distclean :: 
	$(MAKE) -C./linux $@

clean :: 
	gnatclean -r -P./collaboration.gpr
	rm -rf $(dist)

distclean :: clean 
	rm -rf ./lib ./build
	find . -name fontconfig | tee | xargs rm -rf 
