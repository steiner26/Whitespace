open Ast

let character_of_number number = 
  try Printf.sprintf " (%C)" (Char.chr number)
  with Invalid_argument _ -> ""
    
let name_of_stmt stmt = 
  match stmt with 
    | Push _ -> "Push" 
    | Copy _ -> "Copy" 
    | Slide _ -> "Slide"
    | Mark _ -> "Mark"
    | Call _ -> "Call"
    | Jump _ -> "Jump"
    | JumpIfZero _ -> "Jump if Zero"
    | JumpIfNegative _ -> "Jump if Negative"
    | Duplicate -> "Duplicate"
    | Swap -> "Swap"
    | Discard -> "Discard"
    | Add -> "Add"
    | Subtract -> "Subtract"
    | Multiply -> "Multiply"
    | Divide -> "Divide"
    | Modulo -> "Modulo"
    | Store -> "Store"
    | Retrieve -> "Retrieve"
    | EndSubroutine -> "End Subroutine"
    | EndProgram -> "End Program"
    | OutputCharacter -> "Output Character"
    | OutputNumber -> "Output Number"
    | ReadCharacter -> "Read Character"
    | ReadNumber -> "Read Number"

let string_of_stmt stmt = 
  let name = name_of_stmt stmt in 
  match stmt with 
    | Push number -> begin
      let character = 
        try Printf.sprintf " (%C)" (Char.chr number) 
        with Invalid_argument _ -> "" in 
      Printf.sprintf "%s %d%s" name number character
    end 
    | Copy number 
    | Slide number -> Printf.sprintf "%s %d" name number
    | Mark label 
    | Call label
    | Jump label
    | JumpIfZero label
    | JumpIfNegative label -> Printf.sprintf "%s %s" name label
    | Duplicate
    | Swap
    | Discard
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulo
    | Store
    | Retrieve
    | EndSubroutine
    | EndProgram
    | OutputCharacter
    | OutputNumber 
    | ReadCharacter 
    | ReadNumber -> name