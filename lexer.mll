{
open Lexing
open Parser
exception LexingError of string

let lineno = ref 1
let colno = ref 1

let incr_lineno () = colno := 1; lineno := succ !lineno

let incr_colno () = colno := succ !colno

let error lexbuf =
  let t = Lexing.lexeme lexbuf in
  let s = Printf.sprintf "line %d, character %d" !lineno (!colno-1) in 
  let err = Printf.sprintf "%s: lexing error at '%s'\n" s t in 
  raise (LexingError err)
}

let space = ' '
let tab = '\t'
let linefeed1 = '\n' 
let linefeed2 = "\r\n"

rule token = parse 
  | space { incr_colno (); SPACE }
  | tab { incr_colno (); TAB }
  | linefeed1 | linefeed2 { incr_lineno (); LINEFEED }
  | eof { EOF }
  | _ {incr_colno (); token lexbuf }