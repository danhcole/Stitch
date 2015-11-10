OBJS = stch_ast.cmo stch_parser.cmo stch_scanner.cmo stch_compiler.cmo stitch.cmo

YACC = ocamlyacc

stitch: $(OBJS)
	ocamlc -o stitch $(OBJS)

stch_scanner.ml: stch_scanner.mll
	ocamllex scanner.mll

stch_parser.ml stch_parser.mli: stch_parser.mly
	$(YACC) stch_parser.mly

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

.PHONY: clean
clean:
	rm -f stitch stch_parser.ml stch_parser.mli stch_scanner.ml \
	    *.cmo *.cmi *.out *.diff *.output

.PHONY: all
all: clean stitch

#stch_ast.cmo:
#stch_ast.cmx:
#stch_compiler.cmo: ast.cmo
#stch_compiler.cmx: ast.cmx
#stitch.cmo: stch_scanner.cmo stch_parser.cmi stch_compiler.cmo ast.cmo
#stitch.cmx: stch_scanner.cmo stch_parser.cmx stch_compiler.cmx ast.cmx
#stch_parser.cmo: stch_ast.cmo stch_parser.cmi
#stch_parser.cmx: stch_ast.cmx stch_parser.cmi
#stch_scanner.cmo: stch_parser.cmi
#stch_scanner.cmx: stch_parser.cmx
#stch_parser.cmi: stch_ast.cmo
stch_ast.cmo :
stch_ast.cmx :
stch_compiler.cmo : stch_ast.cmo
stch_compiler.cmx : stch_ast.cmx