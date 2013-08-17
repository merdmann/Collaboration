#!/bin/sh

function value () {
	tmp=`fgrep "$1=" central.cfg | cut -d "=" -f 2 | cut -d ";" -f 1`
	echo $tmp
}

m1=`value P1 | cut -d',' -f 2`  
m2=`value P2 | cut -d',' -f 2`  


make central
gnuplot <<EOF
set output "x.png"
set terminal png

set title "Central elastic collision M1=${m1}, M2=${m2}"
set xlabel "Time"
set ylabel "X"

plot '/tmp/brad-x86_64-bachus.site.stdout-1.txt' using 4 title 'M1' with lines,'/tmp/brad-x86_64-bachus.site.stdout-2.txt' using 4 title 'M2' with lines
EOF

