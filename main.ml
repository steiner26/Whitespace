open Ast
open Lexer

let () = 
  let _ = 
    if Array.length Sys.argv < 2 || Array.length Sys.argv > 3 then 
      (Printf.printf "Usage: ./whitespace [file] <option>\n";
       exit 0) in
  let filename = Sys.argv.(1) in
  let _ = if Array.length Sys.argv = 3 && Sys.argv.(2) = "--debug" then 
    ignore (Parsing.set_trace true);
    Printexc.record_backtrace true; in 
  let lexbuf = 
    try Lexing.from_channel (open_in filename) 
    with e -> 
      match e with 
        | Sys_error s -> Printf.printf "%s\n" s; exit 1
        | _ -> let msg = Printexc.to_string e in 
          Printf.printf "Whitespace: Unexpected error occured - %s\n" msg; exit 1  
    in
  let statements = 
    try Parser.program Lexer.token lexbuf 
    with e -> 
      match e with 
        | LexingError msg -> Printf.printf "Whitespace: %s" msg; exit 1
        | Parsing.Parse_error -> let stack = Printexc.get_backtrace () in 
          Printf.printf "Whitespace: Parse_error\n%s" stack; exit 1
        | _ -> let msg = Printexc.to_string e in 
          Printf.printf "Whitespace: Unexpected error occured - %s\n" msg; exit 1  
    in 
  Printf.printf "%B\n" (statements = [EndProgram (1, 1)])