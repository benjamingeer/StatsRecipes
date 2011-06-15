PROGRAM = summary
PROGRAM_SRC = Summary.hs
DATA_FILE = test-data.csv

run: $(PROGRAM)
	./$(PROGRAM) $(DATA_FILE)

$(PROGRAM): $(PROGRAM_SRC)
	ghc --make $< -o $@

clean:
	rm -f *.hi *.o $(PROGRAM) *~
