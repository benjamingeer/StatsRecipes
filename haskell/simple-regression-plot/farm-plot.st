set terminal epslatex color
set o "$outputFile$"
f(x) = $intercept$ + $slope$ * x
set style line 1 pointtype 7 linecolor rgb "blue"
set style line 2 linetype 1 linecolor rgb "red"
set xlabel "Year"
set ylabel "Millions of Persons"
set lmargin 0
plot "$dataFile$" using "%lf,%lf" linestyle 1 title "Population", f(x) linestyle 2 title "Regression"
