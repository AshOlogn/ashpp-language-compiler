build:
	dune build @install && dune build @install src/main.exe

run:
	dune exec src/main.exe

clean:
	dune clean

.PHONY: build run