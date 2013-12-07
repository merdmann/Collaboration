PATH:=/opt/gnat/bin:${PATH}
SHELL:=bash

tmp=/tmp
dist=collaboration-1.0.1
home=$(shell pwd)
os=$(shell uname)
ws=$(tmp)/$(dist)


CP=cp
RM=rm -rf
MKDIR=mkdir -p

## default target
all : dirs machdeps library 

## work directories
dirs : ./lib ./build

./lib ./build :
	$(MKDIR) $@

## build the operating system dep parts
machdeps : $(os)

Linux:
	$(MAKE) -DLinux -C./linux

CYGWIN_NT-6.2 Windows_NT :
	$(MAKE) -C./linux

## the actual library
library : 
	gprbuild -P./collaboration.gpr


## build distribution
$(ws):
	$(RM) $(ws)
	$(MKDIR) $(ws)

dist : distclean $(ws)
	$(CP) -a * $(ws)
	( cd $(ws)/../ && tar cvf $(home)/$(dist)-src.tar --exclude=\*release\* --exclude-vcs $(dist) )
	$(RM) $(ws)
	gzip $(dist)-src.tar

## cleanup
clean distclean :: 
	$(MAKE) -C./linux $@
	$(MAKE) -C./examples/particles $@


clean :: dirs 
	gnatclean -r -P./collaboration.gpr
	$(RM) $(dist)

distclean :: clean 
	$(RM) ./lib ./build
	find . -name fontconfig | tee | xargs rm -rf 
