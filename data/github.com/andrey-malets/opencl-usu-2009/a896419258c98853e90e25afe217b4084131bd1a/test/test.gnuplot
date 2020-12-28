set terminal jpeg size 1024,768
set output "threshold.jpeg"
set style data histogram
set style histogram clustered errorbars gap 1 
set style fill solid

set key left
set grid

set yrange[0:5.5]

set title "Threshold tests"
set xlabel "Dimension, width x height"
set ylabel "Time, .sec"

set xtics	("4000x3500" 0, "4500x4000" 1, "5000x4500" 2, "6000x5500" 3)

plot newhistogram "", "data/ThresholdOpencl.data" using 1:2 title "OpenCL", \
						"data/ThresholdCpu.data" using 1:2 title "CPU"

reset

set terminal jpeg size 1024,768
set output "linear.jpeg"

set style data histogram
set style histogram clustered errorbars gap 1 
set style fill solid
set key left

set grid

set title "Linear tests"
set xlabel "Dimension, width x height"
set ylabel "Time, .sec"

set xtics	("4000x3500" 0, "4500x4000" 1, "5000x4500" 2, "6000x5500" 3)

plot newhistogram "", 	"data/LinearOpencl.data" using 1:2 title "OpenCL", \
						"data/LinearCpu.data" using 1:2 title "CPU"
						
reset

set terminal jpeg size 1024,768
set output "gauss.jpeg"

set style data histogram
set style histogram clustered errorbars gap 1 
set style fill solid
set key left

set grid

set title "Gauss tests with param (4, 12)"
set xlabel "Dimension, width x height"
set ylabel "Time, .sec"

set xtics	("320x240" 0, "640x480" 1, "1024x768" 2, "1280x1024" 3);

plot newhistogram "", 	"data/GaussOnencl_4_12.data" using 1:2 title "OpenCL", \
						"data/GaussCpu_4_12.data" using 1:2 title "CPU"
						
