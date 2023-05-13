.PHONY: test check

test:
	OCAMLRUNPARAM=b dune exec test/main.exe