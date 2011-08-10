TEX = pdflatex --shell-escape

PLOT_DATA = farm.csv
PLOT = farm-plot.eps
PLOTTER = farm.py
LATEX_DOC = farm-python.tex
LATEX_OUT = farm-python.pdf

INFERENCE = crying.py
INFERENCE_DATA = crying.csv

all: $(LATEX_OUT) $(INFERENCE)
	@echo
	./$(INFERENCE) $(INFERENCE_DATA)

$(LATEX_OUT): $(LATEX_DOC) $(PLOT)

$(PLOT): $(PLOT_DATA) $(PLOTTER)
	./$(PLOTTER) --data=$(PLOT_DATA) --output=$(PLOT)

%.pdf: %.tex
	$(TEX) $<
	$(TEX) $<

clean:
	rm -f *.aux *.log *.eps *.pdf *~
