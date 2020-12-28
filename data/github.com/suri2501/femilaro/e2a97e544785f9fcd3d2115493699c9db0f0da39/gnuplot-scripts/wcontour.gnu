infile = 'el-pena.dat'
outfile = 'el-pena.eps'


# To select different line styles we prepare various temporary data files

# set some sampling parameters
set isosamples 10, 10
set samples 10, 10
set cntrparam bspline

# don't plot any 3D surface
unset surface
set contour base

# set the contour levels
set cntrparam levels discrete -0.8 , -0.5 , -0.3 , -0.1

# Make the plot using the first data set as X, the second as Y and the
# sixth one as contour data.
set table 'negative.dat'
splot infile using 1:2:6
unset table

# set the contour levels
set cntrparam levels discrete 0.0

set table 'zero.dat'
splot infile using 1:2:6
unset table

# set the contour levels
set cntrparam levels discrete 0.1 , 0.3 , 0.5 , 0.8

set table 'positive.dat'
splot infile using 1:2:6
unset table


# Now we are ready to make the plots


set terminal postscript eps enhanced font 'Helvetica,18'
set output outfile

# set the viewpoint "from top"
set view map

# Avoid labels and keys
unset clabel
unset key

set xrange [-5:20]
set yrange [-8:8]

set style line 1 lt 3 lw 3
set style line 2 lt 1 lw 1
set style line 3 lt 1 lw 3
set style increment userstyle

# data will be represented by lines
set style data lines

plot 'negative.dat' lt 1, 'zero.dat' lt 2, 'positive.dat' lt 3

