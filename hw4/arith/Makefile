all:
	ocamllex arithLexer.mll
	ocamlyacc arithParser.mly
	ocamlc -c arith.ml
	ocamlc -c arithParser.mli
	ocamlc -c arithLexer.ml
	ocamlc -c arithParser.ml
	ocamlc -c main.ml

clean:
	rm -f *.cm[io] arithLexer.ml arithParser.ml arithParser.mli
