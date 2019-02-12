build:
	dune build @install src && dune build main.exe && dune build tests

run:
	./_build/default/main.exe

test:
	dune runtest

clean:
	dune clean

.PHONY: build run test