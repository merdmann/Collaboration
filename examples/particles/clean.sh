#!/bin/sh
( echo "P,T,M,X,Y,Z,VX,VY,VZ"
fgrep -v /usr/bin/time $1 | fgrep "P $2" | tr -d 'P' | tr ';' ',' ) > $1-$2.txt


