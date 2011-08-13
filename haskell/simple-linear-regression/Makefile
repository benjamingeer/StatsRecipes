TEX = pdflatex --shell-escape

PLOT_DATA = farm.csv
PLOT = farm-plot.tex
PLOTTER = farm
PLOTTER_SRC = Farm.hs
GNUPLOT_SCRIPT_TEMPLATE = farm-plot.st
GNUPLOT_SCRIPT = farm-plot.gp
LATEX_DOC = farm-haskell.tex
LATEX_OUT = farm-haskell.pdf

INFERENCE_SRC = Crying.hs
INFERENCE = crying
INFERENCE_DATA = crying.csv

all: $(LATEX_OUT) $(INFERENCE)
	@echo
	./$(INFERENCE) $(INFERENCE_DATA)

$(LATEX_OUT): $(LATEX_DOC) $(PLOT)

$(PLOTTER): $(PLOTTER_SRC)
	ghc --make $< -o $@

$(PLOT): $(PLOT_DATA) $(PLOTTER) $(GNUPLOT_SCRIPT_TEMPLATE)
	./$(PLOTTER) --data=$(PLOT_DATA) \
		--template=$(GNUPLOT_SCRIPT_TEMPLATE) \
		--script=$(GNUPLOT_SCRIPT) --output=$(PLOT)
	gnuplot $(GNUPLOT_SCRIPT)

%.pdf: %.tex
	$(TEX) $<
	$(TEX) $<

$(INFERENCE): $(INFERENCE_SRC)
	ghc --make $< -o $@

clean:
	rm -f *.aux *.log *.eps *.pdf *.hi *.o $(PLOT) $(PLOTTER) $(GNUPLOT_SCRIPT) $(INFERENCE) *~
