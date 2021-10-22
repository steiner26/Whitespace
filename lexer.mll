{
open Lexing
open Parser
exception LexingError of string

let lineno = ref 1
let colno = ref 1

let process_token token = 
  let () = match token with 
    | SPACE _ 
    | TAB _ ->  colno := succ !colno;
    | LINEFEED _ -> colno := 1; lineno := succ !lineno;
    | EOF -> () in 
    token
    

let incr_lineno () : unit = 
  lineno := succ !lineno

let info () = 
  (!lineno, !colno)

let error lexbuf =
  let (l, c) = info () in
  let t = Lexing.lexeme lexbuf in
  let s = Printf.sprintf "line %d, character %d" l (c-1) in 
  let err = Printf.sprintf "%s: lexing error at '%s'\n" s t in 
  raise (LexingError err)
}

let space = ' '
let tab = '\t'
let linefeed = '\n'

rule token = parse 
  | space { process_token (SPACE (info ())) }
  | tab { process_token (TAB (info ())) }
  | linefeed { process_token (LINEFEED (info ())) }
  | eof { process_token EOF }
  | _ { error lexbuf }