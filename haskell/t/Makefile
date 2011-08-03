PROGRAM = newts
PROGRAM_SRC = Newts.hs
DATA_FILE = newts.csv

run: $(PROGRAM)
	./$(PROGRAM) $(DATA_FILE)

$(PROGRAM): $(PROGRAM_SRC)
	ghc --make $< -o $@

clean:
	rm -f *.hi *.o $(PROGRAM) *~
