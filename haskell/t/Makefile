PROGRAM_1 = newts
PROGRAM_1_SRC = Newts.hs
DATA_1 = newts.csv

PROGRAM_2 = cola
PROGRAM_2_SRC = Cola.hs
DATA_2 = cola.csv

PROGRAM_3 = fabric
PROGRAM_3_SRC = Fabric.hs
DATA_3 = fabric1.csv fabric2.csv

run: $(PROGRAM_1) $(PROGRAM_2) $(PROGRAM_3)
	./$(PROGRAM_1) $(DATA_1)
	@echo
	./$(PROGRAM_2) $(DATA_2)
	@echo
	./$(PROGRAM_3) $(DATA_3)

$(PROGRAM_1): $(PROGRAM_1_SRC)
	ghc --make $< -o $@

$(PROGRAM_2): $(PROGRAM_2_SRC)
	ghc --make $< -o $@

$(PROGRAM_3): $(PROGRAM_3_SRC)
	ghc --make $< -o $@

clean:
	rm -f *.hi *.o $(PROGRAM_1) $(PROGRAM_2) $(PROGRAM_3) *~
