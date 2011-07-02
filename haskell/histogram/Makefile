TEX = pdflatex --shell-escape

DATA = degree.csv
PLOT = degree-plot.tex
PLOTTER = degree.gp
DOC = degree-gnuplot.tex
OUT = degree-gnuplot.pdf

$(OUT): $(DOC) $(PLOT)

$(PLOT): $(DATA) $(PLOTTER)
	gnuplot $(PLOTTER)

%.pdf: %.tex
	$(TEX) $<
	$(TEX) $<

clean:
	rm -f $(PLOT) *.aux *.log *.eps *.pdf *~
