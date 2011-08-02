PROGRAM = normal
PROGRAM_SRC = Normal.hs

run: $(PROGRAM)
	./$(PROGRAM)

$(PROGRAM): $(PROGRAM_SRC)
	ghc --make $< -o $@

clean:
	rm -f *.hi *.o $(PROGRAM) *~
