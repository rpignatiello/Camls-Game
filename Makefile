.PHONY: test check

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play: 
	OCAMLRUNPARAM=b dune exec frontend/front.exe
