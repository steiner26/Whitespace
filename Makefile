UNITS = parser pretty lexer ast eval main
MAIN = whitespace
OBJECTS = $(UNITS:=.cmo) 
ARTIFACTS = lexer.ml parser.ml parser.mli parser.output

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

$(MAIN): $(OBJECTS)
	ocamlc -o $@ $^

lexer.ml: lexer.mll
	ocamllex -q $<

parser.ml: parser.mly
	ocamlyacc -v $<

parser.mli: parser.mly
	ocamlyacc -v $<

clean:
	rm -f *.cmo *.cmi $(ARTIFACTS)

deps: 
	ocamldep -bytecode *.mli *.ml *.mll *.mly
	
ast.cmo :
eval.cmo : ast.cmo
lexer.cmo : parser.cmi
main.cmo : pretty.cmo parser.cmi lexer.cmo eval.cmo ast.cmo
parser.cmo : ast.cmo parser.cmi
parser.cmi : ast.cmo
pretty.cmo : ast.cmo