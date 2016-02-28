# PAOLINI DANIELE - MAT. 444260
# Interpreter makefile

# variables
SHELL = /bin/bash

# fake targets
.PHONY : install
.PHONY : clean
.PHONY : test
.PHONY : spec-test

all :
	@echo "==> Setting environment..."
	@ocamlc interpreter.ml assert.ml interpreter_test.ml -o Test
	@echo "==> Done!"

spec : interpreter.ml assert.ml interpreter_test.ml
	@echo "==> Setting environment..."
	@ocamlopt interpreter.ml assert.ml interpreter_test.ml -o Test
	@echo "==> Done!"

install : all

test : interpreter_test.ml
	@echo "==> Starting test..."
	@ocamlrun Test
	@echo "==> Done!"

spec-test : interpreter_test.ml
	@echo "==> Starting test..."
	@./Test
	@echo "==> Done!"

clean :
	@echo "==> Cleaning environment..."
	@rm -f *.cmi *.cmo *.cmx *.o Test
	@echo "==> Done!"
