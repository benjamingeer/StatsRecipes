TEX = pdflatex --shell-escape

DATA = degree.csv
PLOT = degree-plot.eps
PLOTTER = degree.py
DOC = degree-python.tex
OUT = degree-python.pdf

$(OUT): $(DOC) $(PLOT)

$(PLOT): $(DATA) $(PLOTTER)
	./$(PLOTTER) --data=$(DATA) --output=$(PLOT)

%.pdf: %.tex
	$(TEX) $<
	$(TEX) $<

clean:
	rm -f *.aux *.log *.eps *.pdf *~
