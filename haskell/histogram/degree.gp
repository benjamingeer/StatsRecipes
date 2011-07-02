set terminal epslatex color
set o "degree-plot.tex"

set key off
set datafile separator ","
set xrange[15:45]
set xlabel "Percent of adults with bachelor's degree"
set ylabel "Number of states"
set lmargin 0

set style line 1 linecolor rgb "blue"
set style increment user
set style fill solid 1 border -1
 
binwidth = 2.5
set boxwidth binwidth
bin(x, width) = width * floor(x / width) + binwidth / 2.0
plot "degree.csv" using (bin($2, binwidth)):(1.0) smooth freq with boxes
