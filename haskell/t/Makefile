PROGRAM_1 = newts
PROGRAM_1_SRC = Newts.hs
DATA_FILE_1 = newts.csv

PROGRAM_2 = cola
PROGRAM_2_SRC = Cola.hs
DATA_FILE_2 = cola.csv

run: $(PROGRAM_1) $(PROGRAM_2)
	./$(PROGRAM_1) $(DATA_FILE_1)
	@echo
	./$(PROGRAM_2) $(DATA_FILE_2)

$(PROGRAM_1): $(PROGRAM_1_SRC)
	ghc --make $< -o $@

$(PROGRAM_2): $(PROGRAM_2_SRC)
	ghc --make $< -o $@

clean:
	rm -f *.hi *.o $(PROGRAM_1) $(PROGRAM_2) *~
