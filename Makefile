default:
	dune build src/tui.exe	

play:
	dune exec src/tui.exe

clean: bisect-clean
	dune clean

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

test:
	OCAMLRUNPARAM=b dune exec testing/main.exe

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force testing/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

zip:
	rm -f creative_chess_ms2.zip
	zip -r creative_chess_ms2.zip . -x@exclude.lst