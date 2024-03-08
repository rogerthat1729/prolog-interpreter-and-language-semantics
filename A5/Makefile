all: clean

clean: run
	@echo "\nCleaning up..."
	@rm -r -f *.o
	@rm -r -f *.out
	@rm -r -f *.cmi
	@rm -r -f *.cmo

run:
	@echo "Compiling...\n"
	@ocamlc -o main.out main.ml
	@echo "Running...\n\nOutput:\n"
	@./main.out