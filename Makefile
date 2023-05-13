.PHONY: test check

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play: 
	OCAMLRUNPARAM=b dune exec frontend/front.exe

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage