#!/gnuplot

FILE_OUT='elec-prot-density-4-8.eps'
set terminal postscript enhanced eps color "Helvetica" 16
set output FILE_OUT

unset key

set tics scale 0.5


set ylabel 'y [{/Symbol l}_0]' 
set xrange [15:25]
set logscale cb
set cbrange [0.001:1]
#set format cb "%L10^{%L}"
#set xtics 45
#set ytics 1
set size ratio -1

set grid front

# Enable the use of macros
set macros
load "palettes.pt"

TMARGIN = "set tmargin at screen 0.90; set bmargin at screen 0.55"
BMARGIN = "set tmargin at screen 0.55; set bmargin at screen 0.20"
LMARGIN = "set lmargin at screen 0.15; set rmargin at screen 0.55"
RMARGIN = "set lmargin at screen 0.42; set rmargin at screen 0.95"
#LMARGIN = "set lmargin at screen 0.15;"
#RMARGIN = "set lmargin at screen 0.4;" 
TLTITLE  = "set title 't=15T' "
BLTITLE  = "set title ''"
TRTITLE  = "set title 't=30T' "
BRTITLE  = "set title '' "
# x- and ytics for each row resp. column
NOXTICS = "set format x '' ; unset xlabel"
XTICS = "set format x '%.0f'; set xtics rotate ; set xlabel 'x [{/Symbol l}_0]' "
NOYTICS = "set format y ''; unset ylabel"
YTICS = "set format y '%.0f'; set ylabel 'y [{/Symbol l}_0]'"

set multiplot layout 2,2
#-----------PLOT elettroni-------------------
unset colorbox #set colorbox user origin .75, .55 size 0.02, 0.3
unset yrange
set yrange [0:10]
set cblabel 'n_e [64 n_c]'
@TMARGIN; @LMARGIN
@NOXTICS; @YTICS
@TLTITLE
#set tmargin at screen 0.90; set bmargin at screen 0.55
@PWHOT
plot "Edenout02_out.2D" u 1:2:(abs($3)) w image t ''

#----------PLOT protoni --------------------
unset colorbox #set colorbox user origin .75, .2 size 0.02, 0.3
@BMARGIN; @LMARGIN
@XTICS; @YTICS
@BLTITLE
set tmargin at screen 0.55; set bmargin at screen 0.20
set yrange [10:0]
set cblabel 'n_p [64 n_c]'
@PWCOLD
plot "Pdenout02_out.2D" u 1:2:(abs($3)) w image t ''


#=========== second column ================
unset xrange
set xrange [15:25]

#-----------PLOT elettroni-------------------
@TRTITLE
set colorbox user origin .775, .575 size 0.02, 0.3
unset yrange
set yrange [0:10]
set cblabel 'n_e [64 n_c]'
@TMARGIN; @RMARGIN
@NOXTICS; @NOYTICS
#set tmargin at screen 0.90; set bmargin at screen 0.55
@PWHOT
plot "Edenout04_out.2D" u 1:2:(abs($3)) w image t ''

#----------PLOT protoni --------------------
@BRTITLE
set colorbox user origin .775, .225 size 0.02, 0.3
@BMARGIN; @RMARGIN
@XTICS; @NOYTICS
set tmargin at screen 0.55; set bmargin at screen 0.20
set yrange [10:0]
set cblabel 'n_p [64 n_c]'
@PWCOLD
plot "Pdenout04_out.2D" u 1:2:(abs($3)) w image t ''


unset multiplot
