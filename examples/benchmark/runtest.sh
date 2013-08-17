#!/bin/sh
PATH=/opt/gnat/bin:$PATH
export PATH

if [ ! -d ../$1 ] ; then
   echo "$1 is not a benchmark"
   exit 1
fi

today=`date +\%Y-\%m-\%d`
now=`date +%H-%M-%S`

result=${today}-${now}.log

pkg=$1
shift

tmp=/tmp/runtest.$$
defs=""
worker=""
rundir=${tmp}

for i in $* ; do
   case "$1" in
    -*=*) optarg=`echo "$1" | sed 's/[-_a-zA-Z0-9]*=//'` ;;
       *) optarg= ;;
   esac

   case $1 in
       	--initial-state=*)
   	   state=$optarg
   	   ;;

       	--N=* )
   	   N=$optarg
   	   ;;

       	--worker=*)
   	   worker=$optarg
   	   ;;

       	*)
	   echo "Error unknown option $1 "
	   exit 1
          ;;
   esac
   shift
done

defs="rundir=${rundir} ${defs}"

procs=`fgrep processor /proc/cpuinfo | wc -l`
if [ "x${worker}" = "x" ] ; then
    procs=`echo ${procs}-1|bc`
else
    procs=${worker}
fi

make -C../${pkg} ${defs} clean
make -C../${pkg} ${defs}
if [ "$?" != "0" ] ; then
	exit 1
fi

if [ "x$N" = "x" ] ; then
	N="800 700 600 500 400 300 200 100 90 80 70 60 50 40 30 20 10"
fi

if [ "x${state}" != "x" ] ; then
	defs="${defs} initial_state=/tmp/${state}"
else
	rm -f state.init
fi

rm -rf xxxx yyyy ${result}
cfg=`pwd`/tmp
for n in $N ; do
   m4 -D_N_=$n -D_W_=${procs} nbody._cfg > ${cfg}.cfg
   ( cd ../${pkg} && ./run.sh ${cfg} xxxx yyyy ) | grep T_ | awk -v N=$n -f result.awk | tee -a ${result}
done
