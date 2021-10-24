{
open Lexing
open Parser
exception LexingError of string

let lineno = ref 1
let colno = ref 1

let incr_lineno () = colno := 1; lineno := succ !lineno

let incr_colno () = colno := succ !colno

let info () = (!lineno, !colno)

let error lexbuf =
  let (l, c) = info () in
  let t = Lexing.lexeme lexbuf in
  let s = Printf.sprintf "line %d, character %d" l (c-1) in 
  let err = Printf.sprintf "%s: lexing error at '%s'\n" s t in 
  raise (LexingError err)
}

let space = ' '
let tab = '\t'
let linefeed1 = '\n' 
let linefeed2 = "\r\n"

rule token = parse 
  | space { incr_colno (); SPACE (info ()) }
  | tab { incr_colno (); TAB (info ()) }
  | linefeed1 | linefeed2 { incr_lineno (); LINEFEED (info ()) }
  | eof { EOF }
  | _ {incr_colno (); token lexbuf }