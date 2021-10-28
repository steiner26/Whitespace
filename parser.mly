%{
open Ast
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
  | sign bits LINEFEED { ($1, $2) }

label:
  | bits LINEFEED { $1 }  

sign: 
  | SPACE { Positive }
  | TAB { Negative }

bits: 
  | bits bit { $2 :: $1 }
  | { [] }

bit: 
  | SPACE { Zero }
  | TAB { One }

