OBJS = stch_ast.cmo stch_parser.cmo stch_scanner.cmo stch_compiler.cmo stitch.cmo

YACC = ocamlyacc

stitch: $(OBJS)
	ocamlc -o stitch $(OBJS)

stch_scanner.ml: stch_scanner.mll
	ocamllex stch_scanner.mll

stch_parser.ml stch_parser.mli: stch_parser.mly
	$(YACC) -v stch_parser.mly

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

.PHONY: clean
clean:
	rm -f stitch stch_parser.ml stch_parser.mli stch_scanner.ml \
	    *.cmo *.cmi *.out *.diff *.output stitch

.PHONY: all
all: clean stitch

stch_ast.cmo :
stch_ast.cmx :
stch_compiler.cmo : stch_ast.cmo
stch_compiler.cmx : stch_ast.cmx
stch_parser.cmo : stch_ast.cmo stch_parser.cmi
stch_parser.cmx : stch_ast.cmx stch_parser.cmi
stch_scanner.cmo : stch_parser.cmi
stch_scanner.cmx : stch_parser.cmx
stitch.cmo :
stitch.cmx :
stch_parser.cmi : stch_ast.cmo
