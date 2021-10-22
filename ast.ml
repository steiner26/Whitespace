type info = (int * int)

type sign = 
  | Positive
  | Negative

type bit = 
  | Zero
  | One

(* bits are stored with the least significant bit at the head of the list *)
type number = sign * bit list
type label = bit list

type 'a stmt = 
| Push of number * 'a 
| Duplicate of 'a
| Copy of number * 'a
| Swap of 'a
| Discard of 'a
| Slide of number * 'a
| Add of 'a
| Subtract of 'a
| Multiply of 'a
| Divide of 'a
| Modulo of 'a
| Store of 'a
| Retrieve of 'a
| Mark of label * 'a
| Call of label * 'a
| Jump of label * 'a
| JumpIfZero of label * 'a
| JumpIfNegative of label * 'a
| EndSubroutine of 'a
| EndProgram of 'a
| OutputCharacter of 'a
| OutputNumber of 'a
| ReacCharacter of 'a
| ReadNumber of 'a

(* statements are stored with the first statement at the head of the list *)
type 'a program = 'a stmt list
