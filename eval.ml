open Ast 

type program_state = {
  stack: int list;
  heap: (int * int) list;
  labels : (string * int) list;
  initial_program : program;
}

type result = 
  | Error of string
  | Continue of program_state
  | ProgramEnd of program_state
  | SubroutineEnd of program_state

let slide list n = 
  let rec slide_helper lst n h = 
    let tail = List.tl lst in 
    if n = 0 then h::lst 
    else slide_helper tail (n-1) h in 
  match list with 
    | [] -> []
    | v::vs -> slide_helper vs n v

let remove list n = 
  let rec remove_helper lst n = 
    if n = 0 then lst 
    else match lst with 
      | [] -> []
      | h::t -> remove_helper t (n-1) in 
  remove_helper list n 

let store v loc heap = 
  match List.assoc_opt loc heap with 
    | None -> (loc, v)::heap
    | Some _ -> (loc, v)::(List.remove_assoc loc heap)

let generate_labels (program : program) = 
  List.mapi (fun i -> function Mark l -> Some (l, i) | _ -> None) program |> 
  List.fold_left (fun accm -> function Some l -> l::accm | None -> accm) []

let rec eval_stmt stmt state is_subroutine =  
  match stmt with 
    | Push num -> Continue { state with stack = num::state.stack }
    | Copy num -> begin
      try match List.nth_opt state.stack num with 
        | None -> Error "Not enough values on stack for Copy command"
        | Some v -> Continue { state with stack = v::state.stack }
      with Invalid_argument _ -> Error "Negative index given to Copy command"
      end
    | Slide num -> begin
        if List.length state.stack < num + 1
        then Error "Not enough values on stack for Slide command"
        else Continue { state with stack = slide state.stack num } 
      end
    | Duplicate -> begin
      match state.stack with 
        | [] -> Error "No value on stack for Duplicate command"
        | v::vs -> Continue { state with stack = v::v::vs }
      end
    | Swap -> begin
      match state.stack with 
        | [] | [_] -> Error "Not enough values on stack for Swap command"
        | v1::v2::vs -> Continue { state with stack = v2::v1::vs }
      end
    | Discard -> begin
      match state.stack with 
        | [] -> Error "No value on stack for Discard command"
        | _::vs -> Continue { state with stack = vs } 
      end
    | Add -> begin
      match state.stack with 
        | [] | [_] -> Error "Not enough values on stack for Add command"
        | v1::v2::vs -> Continue { state with stack = (v2 + v1)::vs } 
      end
    | Subtract -> begin
      match state.stack with 
        | [] | [_] -> Error "Not enough value son stack for Subtract command"
        | v1::v2::vs -> Continue { state with stack = (v2 - v1)::vs } 
      end
    | Multiply -> begin
      match state.stack with 
        | [] | [_] -> Error "Not enough values on stack for Multiply command"
        | v1::v2::vs -> Continue { state with stack = (v2 * v1)::vs } 
      end
    | Divide -> begin
      match state.stack with 
        | [] | [_] -> Error "Not enough values on stack for Divide command"
        | v1::v2::vs -> 
        try Continue { state with stack = (v2 / v1)::vs } 
        with Division_by_zero -> Error "Division by 0"
      end
    | Modulo -> begin
      match state.stack with 
        | [] | [_] -> Error "Not enough values on stack for Modulo command"
        | v1::v2::vs -> 
        try Continue { state with stack = (v2 mod v1)::vs } 
        with Division_by_zero -> Error "Modulo by 0"
      end
    | Store -> begin
      match state.stack with 
        | [] | [_] ->  Error "Not enough values on stack for Store command"
        | v::loc::vs -> Continue { state with stack = vs; heap = store v loc state.heap }
      end
    | Retrieve -> begin
      match state.stack with 
        | [] -> Error "No value on stack for Retrieve command"
        | loc::vs -> match List.assoc_opt loc state.heap with 
          | None -> Error "Invalid location for Retrieve command"
          | Some v -> Continue { state with stack = v::vs } 
      end
    | OutputCharacter -> begin
      match state.stack with 
        | [] -> Error "No value on stack for Output Character command"
        | v::vs -> try print_char (Char.chr v); flush stdout; Continue { state with stack = vs } 
          with Invalid_argument _ -> Error "Value of Character is not valid"
      end
    | OutputNumber -> begin
      match state.stack with 
        | [] -> Error "No value on stack for Output Character command"
        | v::vs -> print_int v; flush stdout; Continue { state with stack = vs } 
      end
    | ReadCharacter -> begin
      match state.stack with 
        | [] -> Error "No value on stack for Read Character command"
        | loc::vs -> try Continue { state with stack = vs; heap = store (input_byte stdin) loc state.heap } 
          with End_of_file -> Error "Error reading character from input"
      end
    | ReadNumber -> begin
      match state.stack with 
        | [] -> Error "No value on stack for Read Number command"
        | loc::vs -> try let value = input_line stdin |> int_of_string in
          Continue { state with stack = vs; heap = store value loc state.heap } 
          with End_of_file -> Error "Error reading number from input"
          | Failure _ -> Error "Value read does not represent a number"
      end
    | Mark label -> Continue state
    | Call label -> begin
      match List.assoc_opt label state.labels with
        | None -> Error "Label does not exist for Call command"
        | Some i -> match eval_subroutine state (remove state.initial_program i) with
          | ProgramEnd _ | Error _ as e -> e
          | SubroutineEnd state' -> Continue state'
          | Continue _ -> Error "Call command did not conclude with end of program or subroutine"
      end
    | Jump label -> begin
      match List.assoc_opt label state.labels with
        | None -> Error "Label does not exist for Jump command"
        | Some i -> jump label state is_subroutine
      end
    | JumpIfZero label -> begin
      match state.stack with 
        | [] -> Error "No value on stack for Jump If Zero command"
        | v::vs -> if v <> 0 then Continue { state with stack = vs } 
          else jump label { state with stack = vs } is_subroutine
      end
    | JumpIfNegative label -> begin
      match state.stack with 
        | [] -> Error "No value on stack for Jump If Zero command"
        | v::vs -> if v >= 0 then Continue { state with stack = vs } 
          else jump label { state with stack = vs } is_subroutine
      end
    | EndSubroutine -> SubroutineEnd state
    | EndProgram -> ProgramEnd state

and jump label state is_subroutine = 
  match List.assoc_opt label state.labels with
    | None -> Error "Label does not exist for Jump If Zero command"
    | Some i -> let eval' =  if is_subroutine then eval_subroutine else eval_program in 
      match eval' state (remove state.initial_program i) with
        | ProgramEnd _   | Error _ as e -> e
        | SubroutineEnd state' as e -> if is_subroutine then e else Continue state'
        | Continue _ -> Error "Jump If Zero command did not conclude with end of program or subroutine"

and eval_subroutine (state : program_state) = function 
  [] -> Error "No End Program command"
  | h::t -> match h with 
    | Push _ 
    | Copy _ 
    | Slide _ 
    | Mark _ 
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
    | OutputCharacter
    | OutputNumber 
    | ReadCharacter 
    | ReadNumber 
    | Call _
    | Jump _
    | JumpIfZero _
    | JumpIfNegative _ -> begin 
      match eval_stmt h state true with
        | Continue state' -> eval_subroutine state' t
        | SubroutineEnd _ | Error _  | ProgramEnd _ as e -> e
      end
    | EndSubroutine -> SubroutineEnd state
    | EndProgram -> ProgramEnd state

and eval_program (state : program_state) = function 
  [] -> Error "No End Program command"
  | h::t -> match h with 
    | Push _ 
    | Copy _ 
    | Slide _ 
    | Mark _ 
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
    | OutputCharacter
    | OutputNumber 
    | ReadCharacter 
    | ReadNumber 
    | Call _
    | Jump _
    | JumpIfZero _
    | JumpIfNegative _ -> begin 
      match eval_stmt h state false with
        | Continue state' -> eval_program state' t
        | SubroutineEnd _ -> Error "End Subroutine command used outside of subroutine"
        | Error _ as e -> e
        | ProgramEnd _ as e -> e
      end
    | EndSubroutine -> Error "End Subroutine command used outside of subroutine"
    | EndProgram -> ProgramEnd state

let eval (program : program) = 
  program |> eval_program 
  {
    stack = [];
    heap = [];
    labels = generate_labels program;
    initial_program = program;
  } 
