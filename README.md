# Whitespace Interpreter

By Brandon Stein

## Usage

- Run `make` from the root directory to generate the executable `whitespace`
- Run the executable using `./whitespace filename`, where `filename` refers to the file containing a whitespace program

The `whitespace` executable will print the program flow from the given file in plain English.
If a value below 128 is pushed onto the stack, the character representation of
that value will also be printed in the output. There is also support for
non-whitespace characters in programs, which will be ignored by the lexer and
treated as comments.

## Development Notes and Important Documents

- Run `ocamldep -bytecode *.mli *.ml` from the repository root to generate dependency lists for the Makefile
- Run `ocamlyacc -v parser.mly` to generate parser.output which can be used to debug the parser
- [OCaml Lexer and Parser Generators](https://ocaml.org/manual/lexyacc.html)
