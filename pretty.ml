open Ast

let value_of_bit = function
  Zero -> 0 | One -> 1

let string_of_sign = function
  Positive -> "" | Negative -> "-"

let magnitude_of_bits = 
let rec helper value accm bits = 
  match bits with 
    | [] -> accm
    | h::t -> helper (value * 2) (accm + ((value_of_bit h) * value)) t in
  helper 1 0

let string_of_label = 
  let split_first_n list n = 
    let rec helper list accm n = 
      match n with 
        | 0 -> (List.rev accm, list)
        | _ -> match list with 
          | [] -> (List.rev accm, [])
          | h::t -> helper t (h::accm) (n-1) 
    in
  helper list [] n in 
  let rec helper accm bits = 
    let (h, t) = split_first_n bits 8 in 
    let value = magnitude_of_bits h in 
    let result = (Printf.sprintf "%c%s" (Char.chr value) accm)  in 
    match t with 
      | [] -> String.escaped result 
      | _ -> helper (Printf.sprintf "%c%s" (Char.chr value) accm)  t
  in helper "" 

let string_of_number ((sign, bits): number) =
  let magnitude = magnitude_of_bits bits in 
  let signum = string_of_sign sign in 
  Printf.sprintf "%s%d" signum magnitude |> String.escaped

let character_of_number ((sign, bits): number) =
  match sign with 
    | Negative -> ""
    | Positive -> 
      let magnitude = magnitude_of_bits bits in 
      if magnitude < 128 then (string_of_label bits) else ""
    
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
    | Push number -> Printf.sprintf "%s %s (%s)" name (string_of_number number) (character_of_number number )
    | Copy number 
    | Slide number -> Printf.sprintf "%s %s" name (string_of_number number)
    | Mark label 
    | Call label
    | Jump label
    | JumpIfZero label
    | JumpIfNegative label -> Printf.sprintf "%s %s" name (string_of_label label)
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