build:
	dune build @install src && dune build main.exe

run:
	./_build/default/main.exe

clean:
	dune clean

.PHONY: build run