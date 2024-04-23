all: clean

clean: run
	@echo "\nCleaning up ... \n"
	@rm -r *.cmi
	@rm -r *.cmo
	@rm -r parser.ml
	@rm -r parser.mli
	@rm -r lexer.ml

run:
	@echo "Building parse tree ... \n"
	@ocamllex lexer.mll  # Generates lexer.ml
	@ocamlyacc parser.mly  # Generates parser.ml and parser.mli
	@ocamlc -c types.mli
	@ocamlc -c parser.mli  # Compiles the parser interface
	@ocamlc -c lexer.ml    # Compiles the lexer
	@ocamlc -c parser.ml   # Compiles the parser
	@ocamlc -c main.ml     # Compiles the main program
	@ocamlc -o main parser.cmo lexer.cmo main.cmo
	@./main
