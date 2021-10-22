UNITS = parser lexer ast main
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

# List of dependencies generated by `ocamldep -bytecode *.mli *.ml`.
ast.cmo :
lexer.cmo : parser.cmi
main.cmo : parser.cmi lexer.cmo ast.cmo
parser.cmo : ast.cmo parser.cmi
parser.cmi : ast.cmo