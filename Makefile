build:
	dune build @install src && dune build main.exe && dune build tests

run:
	./_build/default/main.exe

clean:
	dune clean

.PHONY: build run