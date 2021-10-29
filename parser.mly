%{
open Ast

let magnitude_of_bits = 
  let rec helper value accm bits = 
    match bits with 
      | [] -> accm
      | h::t -> helper (value * 2) (accm + h * value) t in
  helper 1 0

let value_of_number (sign, bits) = 
  sign * magnitude_of_bits bits

let string_of_label = 
  let split_first_n list n = 
    let rec split_helper list accm n = 
      match n with 
        | 0 -> (List.rev accm, list)
        | _ -> match list with 
          | [] -> (List.rev accm, [])
          | h::t -> split_helper t (h::accm) (n-1) in
    split_helper list [] n in 
  let rec string_of_label_helper accm bits = 
    let (h, t) = split_first_n bits 8 in 
    let value = magnitude_of_bits h in 
    let result = (Printf.sprintf "%c%s" (Char.chr value) accm) in 
    match t with 
      | [] -> String.escaped result 
      | _ -> string_of_label_helper (Printf.sprintf "%c%s" (Char.chr value) accm) t
  in string_of_label_helper "" 
%}

%token SPACE 
%token TAB 
%token LINEFEED
%token EOF

%type <Ast.program> program

%start program 

%%

program: 
  | stmt_list EOF { $1 }

stmt_list:
  | stmt stmt_list { $1 :: $2 }
  | { [] }
  
stmt:
  | stack_manipulation_stmt { $1 }
  | arithmetic_stmt { $1 } 
  | heap_access_stmt { $1 }
  | flow_controll_stmt { $1 }
  | io_stmt { $1 }

stack_manipulation_stmt:
  | SPACE SPACE number { Push $3 }
  | SPACE LINEFEED SPACE { Duplicate }
  | SPACE TAB SPACE number { Copy $4 }
  | SPACE LINEFEED TAB { Swap }
  | SPACE LINEFEED LINEFEED { Discard }
  | SPACE TAB LINEFEED number { Slide $4 }

arithmetic_stmt:
  | TAB SPACE SPACE SPACE { Add }
  | TAB SPACE SPACE TAB { Subtract }
  | TAB SPACE SPACE LINEFEED { Multiply }
  | TAB SPACE TAB SPACE { Divide }
  | TAB SPACE TAB TAB { Modulo }

heap_access_stmt:
  | TAB TAB SPACE { Store }
  | TAB TAB TAB { Retrieve }

flow_controll_stmt:
  | LINEFEED SPACE SPACE label { Mark $4 }
  | LINEFEED SPACE TAB label { Call $4 }
  | LINEFEED SPACE LINEFEED label { Jump $4 }
  | LINEFEED TAB SPACE label { JumpIfZero $4 }
  | LINEFEED TAB TAB label { JumpIfNegative $4 }
  | LINEFEED TAB LINEFEED { EndSubroutine }
  | LINEFEED LINEFEED LINEFEED { EndProgram }

io_stmt:
  | TAB LINEFEED SPACE SPACE { OutputCharacter }
  | TAB LINEFEED SPACE TAB { OutputNumber }
  | TAB LINEFEED TAB SPACE { ReadCharacter }
  | TAB LINEFEED TAB TAB { ReadNumber }

number: 
  | sign bits LINEFEED { value_of_number ($1, $2) }

label:
  | bits LINEFEED { string_of_label $1 }  

sign: 
  | SPACE { 1 }
  | TAB { -1 }

bits: 
  | bits bit { $2 :: $1 }
  | { [] }

bit: 
  | SPACE { 0 }
  | TAB { 1 }

