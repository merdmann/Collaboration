PATH:=/opt/gnat/bin:${PATH}
SHELL:=bash

tmp:=/tmp
dist:=collaboration-1.0.1
home:=$(shell pwd)
os:=$(shell uname)
ws:=$(tmp)/$(dist)


CP=cp
RM=rm -rf
MKDIR=mkdir -p

## default target
all : dirs $(os)
	gnatmake -P./collaboration.gpr

## work directories
dirs : ./lib ./build ./bin

./lib ./build  ./bin :
	$(MKDIR) $@

## os dependand stuff
Linux:
	$(MAKE) Linux=Yes -C./machdeps

CYGWIN_NT-6.2 Windows_NT :
	$(MAKE) -C./machdeps


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
	$(MAKE) -C./machdeps $@
	$(MAKE) -C./examples/particles $@


clean :: dirs 
	-gnatclean -r -P./collaboration.gpr
	$(RM) $(dist)

distclean :: clean 
	$(RM) ./lib ./build
	find . -name fontconfig | tee | xargs rm -rf 
