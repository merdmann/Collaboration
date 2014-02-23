arch=$(shell uname -p)

ifeq ( $(arch),x86_64)
_NUMA_=1
else
_NUMA_=0
endif

## test suite
project=threads

## the place where the programm is executed
rundir=$(shell pwd)
## the place where the program keeps the state information
ROOT=.
CFG=brad
state=state
scheduling=SCHED_BATCH

## internal stuff
arch=$(shell uname -p)
host=$(shell hostname)
date=$(shell date +%s)

exec=particle

bindir=$(rundir)/bin-$(arch)
objdir=$(rundir)/obj-$(arch)
resultdir=$(rundir)/result-$(CFG)

log=$(CFG)-$(arch)-$(host)

tmp=/tmp/$(log)


loginfo=$(tmp).log
cpuinfo=$(tmp).cpu
timeinfo=$(tmp).time
build=$(tmp).build
stdout=$(tmp).stdout
result=$(tmp).result
calc=$(tmp).calc

scheduling=SCHED_BATCH
counter=lockops
algorithm=scattered
sync=barrier

DEFS=-XBINDIR=$(bindir) 	\
	-XRUNDIR=$(rundir)	\
	-XARCH=$(arch) 		\
	-XMAIN=$(exec) 		\
	-XCOUNTER=$(counter) 	\
	-XALGORITHM=$(algorithm) \
	-XSCHEDULING=$(scheduling) \
	-XSYNC=$(sync)

ifeq (_NUMA_,1)
LIBS += -lnuma
CFLAGS += -D_NUMA_=$(_NUMA_)
endif

LIBS += -L$(rundir) -lmachdep

args=--config=$(ROOT)/$(CFG) --logfile=$(loginfo) --state=$(ROOT)/$(state)
ifdef initial_state
args += --initial-state=$(initial_state)
endif

## build executable $(exec)
all build :: dirs $(rundir)/libmachdep.a
	gprbuild -aP . -gnat2012 $(DEFS) -P ./$(project).gpr \
		-cargs -fomit-frame-pointer -gnateDSCHED_POLICY=$(scheduling) \
		-largs $(LIBS)

dirs :: $(bindir) $(objdir)

$(bindir):
	mkdir -p $(bindir)

$(objdir):
	mkdir -p $(objdir)

## machine/os depenant parts
$(rundir)/libmachdep.a ::
	$(MAKE) rundir=$(rundir) CFLAGS=$(CFLAGS) -C./machdep all

clean distclean ::
	$(MAKE) -C./machdep $@

## run a test with a given configuration and collect all relevant configuration data
test ::  $(resultdir)
	@echo "Build Options: $(DEFS)" > $(build)
	$(MAKE) rundir=$(rundir) BUILD=$(BUILD) all >> $(build)
	/usr/bin/time -p -o $(timeinfo) ./setup.sh $(args) --exec=$(bindir)/$(exec) $(args) | tee $(stdout)
	egrep ^P $(stdout) > $(calc)
	cat /proc/cpuinfo | grep "model name" > $(cpuinfo)
	cat /proc/cpuinfo | grep  MHz >> $(cpuinfo)
	@echo "----------------- Processor Information: " > $(result)
	cat $(cpuinfo) >> $(result)
	echo "----------------- Excution Time:" >> $(result)
	cat $(timeinfo) >> $(result)
	fgrep T_ $(loginfo) | grep Total: >> $(result)
	@echo "----------------- Stdout:" >> $(result)
	cat $(stdout) >> $(result)
	@echo "------------------ Build Logging:" >> $(result)
	cat $(build) >> $(result)
	@echo "------------------ Program Logging:" >> $(result)
	cat $(loginfo) >> $(result)
	mv $(result) $(resultdir)/$(log)-$(date).log
	mv $(calc) $(resultdir)/$(log)-$(date)-$(algorithm).calc
	cp $(ROOT)/$(CFG).cfg $(resultdir)/$(CFG)-$(date).cfg

nbody :: $(resultdir)
	$(MAKE) CFG=nbody algorithm=$(algorithm) sync=$(sync) test

$(resultdir):
	mkdir -p $(resultdir)

## just clean local objects
clean ::
	gnatclean -v $(DEFS) -P $(project).gpr
	rm -f $(state).init
	rm -rf *.o a.out

## clean all artefacts including log files
distclean :: clean
	rm -rf $(rundir)/bin-* $(rundir)/obj-*
	rm -rf *.init
	rm -rf *.log *.result
