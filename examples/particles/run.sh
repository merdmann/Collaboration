#!/bin/sh
# ************************************************************************ --
# *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
# *****               FOR  CLASSICAL PARTICLES                       ***** --
# ************************************************************************ --
#                                                                         
# Copyright (C) 2013 Michael Erdmann                                      
#                                                                           
# PSIM is copyrighted by the persons and institutions enumerated in the   
# AUTHORS file. This file is located in the root directory of the          
# PSIM distribution.                                                      
#                                                                          
# PSIM is free software;  you can redistribute it  and/or modify it under 
# terms of the  GNU General Public License as published  by the Free Soft- 
# ware  Foundation;  either version 2,  or (at your option) any later version. 
# PSIM is distributed in the hope that it will be useful, but WITHOUT 
# ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY 
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
# for  more details.  
# You should have  received  a copy of the GNU General Public License  
# distributed with PSIM;  see file COPYING.  If not, write to the 
# Free Software Foundation,  59 Temple Place - Suite 330,  Boston, 
# MA 02111-1307, USA.                                                     
#
#

tmp=/tmp/run.$$
trap 'rm -f "${tmp}" >/dev/null 2>&1' 0
trap "exit 2" 1 2 3 15

root=`pwd` 
config=""

## convert to paraview format
topv() {
( echo "P,T,M,X,Y,Z,VX,VY,VZ"
  fgrep -v /usr/bin/time $1 | fgrep "P $2" | tr -d 'P' | tr ';' ',' ) > $1-$2.txt
}


for i in $* ; do
   case "$1" in
    -*=*) optarg=`echo "$1" | sed 's/[-_a-zA-Z0-9]*=//'` ;;
       *) optarg= ;;
   esac

   case $1 in
       --root=*)
	   root=$optarg
	   ;;
           
       --config=*)
	   config=$optarg
	   ;;

       *) 
           if [ "x${with}" = "x" ] ; then
              with="$1"
           else
              with="${with} $1"
           fi
           ;;
   esac
   shift
done

## create test emvironment
wd=${root}/test
mkdir -p ${wd}

log=${wd}/${config}.log
stdout=${config}.data
 
cp ${config}.cfg ${wd}
cp ./bin/particle ${wd} 

cd ${wd} 
./particle ${config} ${log} ${with} |grep "P " > ${stdout}
topv ${stdout} 1
topv ${stdout} 2
pwd
