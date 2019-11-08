set terminal epslatex color size 4in,2.5in
set key right Right bottom
set xrange [-0.8:0.8]
set yrange [-0.8:0]
w_c = 0.7
d_c = 0.7
unset xtics
set x2tics ("-$w_c$" -w_c,  "0" 0, "$w_c$" w_c)
set ytics ("0" 0, "$d_c$" -d_c)
set x2label "Row position ($x$)"
set ylabel "Depth ($z$)\\vspace{-2.0cm}"
set output "row.tex"
plot "-" title "$L_m$" with lines ls 1, "-" title "$L\\mbox{-isoline}$" with lines ls 2, "-" notitle with lines ls 2, "-" notitle with lines ls 2, "-" title "$L_{0,0}$" with points ls 3 ps 2
-0.7 0
0 -0.7
0.7 0
e

-0.6 0
0 -0.6 
0.6 0
e

-0.4 0
0 -0.4
0.4 0
e

-0.2 0
0 -0.2
0.2 0
e

0 0
e



