PROGRAM = binomial
PROGRAM_SRC = BinomProb.hs

run: $(PROGRAM)
	./$(PROGRAM)

$(PROGRAM): $(PROGRAM_SRC)
	ghc --make $< -O -funbox-strict-fields -o $@

clean:
	rm -f *.hi *.o $(PROGRAM) *~
