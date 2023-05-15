.PHONY: test check

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play: 
	OCAMLRUNPARAM=b dune exec frontend/front.exe

doc: 
	dune build @doc; 
	open _build/default/_doc/_html/index.html