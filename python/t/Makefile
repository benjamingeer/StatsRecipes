PROGRAM_1 = newts.py
DATA_1 = newts.csv

PROGRAM_2 = cola.py
DATA_2 = cola.csv

PROGRAM_3 = fabric.py
DATA_3 = fabric1.csv fabric2.csv

run: $(PROGRAM_1) $(PROGRAM_2)
	./$(PROGRAM_1) $(DATA_1)
	@echo
	./$(PROGRAM_2) $(DATA_2)
	@echo
	./$(PROGRAM_3) $(DATA_3)

clean:
	rm -f *~
