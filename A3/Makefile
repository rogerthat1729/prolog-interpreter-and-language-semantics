all: clean

clean: run
	@rm -f a
	@rm -f *.cmo *.cmi
	@rm -f lex.ml

run:
	@echo "Generating lex.ml..."
	@ocamllex lex.mll
	@echo "\nCompiling and running...\n"
	@ocamlc lex.ml -o a
	@./a