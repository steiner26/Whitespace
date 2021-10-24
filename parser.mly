%{
open Ast
%}

%token <Ast.info> SPACE 
%token <Ast.info> TAB 
%token <Ast.info> LINEFEED
%token EOF

%type <Ast.info Ast.program> program

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
  | SPACE SPACE number { Push ($3, $1) }
  | SPACE LINEFEED SPACE { Duplicate ($1) }
  | SPACE TAB SPACE number { Copy ($4, $1) }
  | SPACE LINEFEED TAB { Swap ($1) }
  | SPACE LINEFEED LINEFEED { Discard ($1) }
  | SPACE TAB LINEFEED number { Slide ($4, $1) }

arithmetic_stmt:
  | TAB SPACE SPACE SPACE { Add ($1) }
  | TAB SPACE SPACE TAB { Subtract ($1) }
  | TAB SPACE SPACE LINEFEED { Multiply ($1) }
  | TAB SPACE TAB SPACE { Divide ($1) }
  | TAB SPACE TAB TAB { Modulo ($1) }

heap_access_stmt:
  | TAB TAB SPACE { Store ($1) }
  | TAB TAB TAB { Retrieve ($1) }

flow_controll_stmt:
  | LINEFEED SPACE SPACE label { Mark ($4, $1) }
  | LINEFEED SPACE TAB label { Call ($4, $1) }
  | LINEFEED SPACE LINEFEED label { Jump ($4, $1) }
  | LINEFEED TAB SPACE label { JumpIfZero ($4, $1) }
  | LINEFEED TAB TAB label { JumpIfNegative ($4, $1) }
  | LINEFEED TAB LINEFEED { EndSubroutine ($1) }
  | LINEFEED LINEFEED LINEFEED { EndProgram ($1) }

io_stmt:
  | TAB LINEFEED SPACE SPACE { OutputCharacter ($1) }
  | TAB LINEFEED SPACE TAB { OutputNumber ($1) }
  | TAB LINEFEED TAB SPACE { ReadCharacter ($1) }
  | TAB LINEFEED TAB TAB { ReadNumber ($1) }

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

