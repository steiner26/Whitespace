type stmt = 
| Push of int 
| Duplicate 
| Copy of int
| Swap 
| Discard 
| Slide of int
| Add 
| Subtract 
| Multiply 
| Divide 
| Modulo 
| Store
| Retrieve 
| Mark of string
| Call of string
| Jump of string
| JumpIfZero of string
| JumpIfNegative of string
| EndSubroutine 
| EndProgram 
| OutputCharacter 
| OutputNumber 
| ReadCharacter 
| ReadNumber 

(* statements are stored with the first statement at the head of the list *)
type  program =  stmt list
