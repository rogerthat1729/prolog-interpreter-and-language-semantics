%{
  open Types
  
  let append a b =
  match a, b with
  | (Termlist l1),(Termlist l2) -> Termlist (l1 @ l2)
  | _,_ -> raise (Invalid_argument "append")
  ;;
%}

%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token <string> ATOM
%token <string> KEYWORD
%token <string> OP
%token <string> STRING
%token Q_DASH
%token DOT COMMA COLON_DASH SEMICOLON PIPE
%token LPAREN RPAREN LSQUARE RSQUARE
%token EOF

%left OP

%start main
%type <Types.termtype> main

%%

main:
  | program EOF
    { $1 }

program:
  | clause DOT
    { Program (Termlist [$1]) }
  | clause DOT program
    { 
      match $3 with
      | Program p -> Program (append (Termlist [$1]) p)
      | _ -> Program (Termlist [$1])
    }

clause:
  | fact
    { Clause $1 }
  | rule
    { Clause $1 }
  | goal
    { Clause $1 }

goal:
  | Q_DASH alist
    { Goal $2 }

fact:
  | head 
    { Fact $1 }
    
rule:
  | head COLON_DASH body 
    { Rule ($1, $3) }

head:
  | atomicformula
    { Head $1 }

body:
  | alist
    { Body $1 }
  
alist:
  | atomicformula
    { Termlist ([$1]) }
  | atomicformula COMMA alist
    { append (Termlist [$1]) $3 }

funct:
  | ATOM LPAREN tlist RPAREN
    { Funct ($1, $3) }

atomicformula:
  | KEYWORD
    { Keyword $1 }
  | ATOM
    { Atom $1 }
  | IDENT OP funda
    { Op ($1, $2, $3)}
  | ATOM LPAREN tlist RPAREN
    { Atomicformula ($1, $3) }

tlist:
  | term
    { Termlist ([$1]) }
  | term COMMA tlist
    { append (Termlist [$1]) $3}

term:
  | funda
    { $1 }
  | funct
    { $1 }

funda:
  | INT
    { Int $1 }
  | BOOL
    { Bool $1 }
  | KEYWORD
    { Keyword $1 }
  | IDENT
    { Variable $1 }
  | STRING
    { String $1 }
  | ATOM
    { Atom $1 }
  | IDENT OP funda
    { Op ($1, $2, $3)}