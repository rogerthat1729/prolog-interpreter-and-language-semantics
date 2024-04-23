%{
  open Types
%}

%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token <string> ATOM
%token <string> KEYWORD
%token <string> OP
%token <string> STRING
%token Q_DASH UNDERSCORE FAIL
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
  | clause DOT  {Program [$1]}
  | clause DOT program 
  {
      match $3 with
    | Program l -> Program ($1::l)
    | _ -> failwith "Parsing Error"
  }

clause:
  | fact {Clause $1}
  | rule {Clause $1}
  | Q_DASH goal {$2}

goal:
  | aflist {Goal $1}

fact:
  | af {Fact $1}

rule:
  | af COLON_DASH aflist {Rule ($1, $3)}

aflist:
  | af {[$1]}
  | af COMMA aflist { $1 :: $3 }

af:
  | ATOM {Atomicformula (Atom($1), [])}
  | ATOM LPAREN tlist RPAREN {Atomicformula (Atom($1), $3)}
  | FAIL {Fail}

tlist:
  | term {[$1]}
  | term COMMA tlist { $1 :: $3 }

term:
  | INT {Int $1}
  | BOOL {Bool $1}
  | UNDERSCORE {Variable "@"}
  // | KEYWORD {Keyword $1}
  | IDENT {Variable $1}
  | STRING {String $1}
  | lst { $1 }
  | af { $1 }

lst:
  | LSQUARE RSQUARE {Nil}
  | LSQUARE lst1 RSQUARE {$2}

lst1:
  | term {List ($1, Nil)}
  | term COMMA lst1 {List ($1, $3)}
  | term PIPE term {List ($1, $3)}

// commalist:
//   | term {List ($1, Nil)}
//   | term COMMA commalist {List ($1, $3)}

// program:
//   | clause DOT
//     { Program (Termlist [$1]) }
//   | clause DOT program
//     { 
//       match $3 with
//       | Program p -> Program (append (Termlist [$1]) p)
//       | _ -> Program (Termlist [$1])
//     }

// clause:
//   | fact
//     { Clause $1 }
//   | rule
//     { Clause $1 }
//   | goal
//     { Clause $1 }

// goal:
//   | Q_DASH alist
//     { Goal $2 }

// fact:
//   | head 
//     { Fact $1 }
    
// rule:
//   | head COLON_DASH body 
//     { Rule ($1, $3) }

// head:
//   | atomicformula
//     { Head $1 }

// body:
//   | alist
//     { Body $1 }
  
// alist:
//   | atomicformula
//     { Termlist ([$1]) }
//   | atomicformula COMMA alist
//     { append (Termlist [$1]) $3 }

// funct:
//   | ATOM LPAREN tlist RPAREN
//     { Funct ($1, $3) }

// atomicformula:
//   | KEYWORD
//     { Keyword $1 }
//   | ATOM
//     { Atom $1 }
//   | IDENT OP funda
//     { Op ($1, $2, $3)}
//   | ATOM LPAREN tlist RPAREN
//     { Atomicformula ($1, $3) }

// tlist:
//   | term
//     { Termlist ([$1]) }
//   | term COMMA tlist
//     { append (Termlist [$1]) $3}

// term:
//   | funda
//     { $1 }
//   | funct
//     { $1 }

// funda:
//   | INT
//     { Int $1 }
//   | BOOL
//     { Bool $1 }
//   | KEYWORD
//     { Keyword $1 }
//   | IDENT
//     { Variable $1 }
//   | STRING
//     { String $1 }
//   | ATOM
//     { Atom $1 }
//   | IDENT OP funda
//     { Op ($1, $2, $3)}