OPTS=--print-types --print-clocks --optimize-ir

input_tast: build
	dune exec ./bin/main.exe -- input.sync --dump-tast $(OPTS)

input_nast: build
	dune exec ./bin/main.exe -- input.sync --dump-nast $(OPTS)

input_ir: build
	dune exec ./bin/main.exe -- input.sync --dump-ir $(OPTS)

input_ml: build
	dune exec ./bin/main.exe -- input.sync $(OPTS)

input: build
	dune exec ./bin/main.exe -- input.sync $(OPTS) > input.ml
	ocamlc -i input.ml

test:
	./test/runner.sh --skip-passed

test_update:
	./test/runner.sh --skip-passed --update

build:
	dune build @all

clean:
	dune clean

all: build

.PHONY: test build clean all
