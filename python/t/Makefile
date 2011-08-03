PROGRAM_1 = newts.py
DATA_FILE_1 = newts.csv

PROGRAM_2 = cola.py
DATA_FILE_2 = cola.csv

run: $(PROGRAM_1) $(PROGRAM_2)
	./$(PROGRAM_1) $(DATA_FILE_1)
	@echo
	./$(PROGRAM_2) $(DATA_FILE_2)

clean:
	rm -f *~
