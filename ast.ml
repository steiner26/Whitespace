type sign = 
  | Positive
  | Negative

type bit = 
  | Zero
  | One

(* bits are stored with the least significant bit at the head of the list *)
type number = sign * bit list
type label = bit list

type stmt = 
| Push of number 
| Duplicate 
| Copy of number
| Swap 
| Discard 
| Slide of number
| Add 
| Subtract 
| Multiply 
| Divide 
| Modulo 
| Store
| Retrieve 
| Mark of label
| Call of label
| Jump of label
| JumpIfZero of label
| JumpIfNegative of label
| EndSubroutine 
| EndProgram 
| OutputCharacter 
| OutputNumber 
| ReadCharacter 
| ReadNumber 

(* statements are stored with the first statement at the head of the list *)
type  program =  stmt list
