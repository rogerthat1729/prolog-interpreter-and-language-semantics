.all : clean

clean: run
	@echo "\nCleaning Up..."
	@rm a.out
	@rm main.cmi
	@rm main.cmo

run:
	@echo "Compiling and running...\n"
	@ocamlc main.ml
	@echo "Output:"
	@./a.out