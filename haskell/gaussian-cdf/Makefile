PROGRAM = cdf
PROGRAM_SRC = CDF.hs

run: $(PROGRAM)
	./$(PROGRAM)

$(PROGRAM): $(PROGRAM_SRC)
	ghc --make $< -o $@

clean:
	rm -f *.hi *.o $(PROGRAM) *~
