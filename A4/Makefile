all: clean

clean: run
	@rm -r _build
	@rm main.native

run:
	@echo "Building parse tree ... \n"
	@ocamlbuild main.native
	@./main.native