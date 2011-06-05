TEX = pdflatex --shell-escape

DATA = farm.csv
PLOT = farm-plot.eps
PLOTTER = farm.py
DOC = farm-python.tex
OUT = farm-python.pdf

$(OUT): $(DOC) $(PLOT)

$(PLOT): $(DATA) $(PLOTTER)
	./$(PLOTTER) --data=$(DATA) --output=$(PLOT)

%.pdf: %.tex
	$(TEX) $<
	$(TEX) $<

clean:
	rm -f *.aux *.log *.eps *.pdf *~
