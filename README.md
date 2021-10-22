# Whitespace Interpreter

By Brandon Stein

## Usage

- Run `make` from the root directory to generate the executable `whitespace`
- Run the executable using `./whitespace filename`, where `filename` refers to the file containing a whitespace program

Currently, the program will output whether the input file contains only the
End Program command (Linefeed Linefeed Linefeed). A sample file `programs/endprogram.ws`
is provided that contains only the End Program command.

## Development Notes and Important Documents

- Run `ocamldep -bytecode *.mli *.ml` from the repository root to generate dependency lists for the Makefile
- Run `ocamlyacc -v parser.mly` to generate parser.output which can be used to debug the parser
- [OCaml Lexer and Parser Generators](https://ocaml.org/manual/lexyacc.html)
