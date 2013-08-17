#!/bin/sh
rm -f scale.log

for i in 4 5 6 7 8 9 10 11 12 13 14 ; do 
 ./runtest.sh psim --N=800 --worker=$i --algorithm=scattered >> scale.log
done
