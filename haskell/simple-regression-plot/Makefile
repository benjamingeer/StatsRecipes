TEX = pdflatex --shell-escape

DATA = farm.csv
PLOT = farm-plot.tex
PLOTTER = farm
PLOTTER_SRC = Farm.hs
GNUPLOT_SCRIPT_TEMPLATE = farm-plot.template
GNUPLOT_SCRIPT = farm-plot.gp
DOC = farm-haskell.tex
OUT = farm-haskell.pdf

$(OUT): $(DOC) $(PLOT)

$(PLOTTER): $(PLOTTER_SRC)
	ghc --make $< -o $@

$(PLOT): $(DATA) $(PLOTTER) $(GNUPLOT_SCRIPT_TEMPLATE)
	./$(PLOTTER) --data=$(DATA) --template=$(GNUPLOT_SCRIPT_TEMPLATE) \
		--script=$(GNUPLOT_SCRIPT) --output=$(PLOT)
	gnuplot $(GNUPLOT_SCRIPT)

%.pdf: %.tex
	$(TEX) $<
	$(TEX) $<

clean:
	rm -f *.aux *.log *.eps *.pdf *.hi *.o $(PLOT) $(PLOTTER) $(GNUPLOT_SCRIPT) *~
