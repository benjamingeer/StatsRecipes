PROGRAM_1 = quality-of-life.py
DATA_1 = quality-of-life.csv

PROGRAM_2 = births.py
DATA_2 = births.csv

run: $(PROGRAM_1) $(PROGRAM_2)
	./$(PROGRAM_1) $(DATA_1)
	@echo
	./$(PROGRAM_2) $(DATA_2)

clean:
	rm -f *~
