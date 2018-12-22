build:
	dune build && dune build src/main.exe

run:
	dune exec src/main.exe

.PHONY: build run