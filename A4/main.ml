open Printf
open Types
  
let rec print_spaces n = 
  match n with
  | 0 -> ()
  | _ -> print_string "  "; print_spaces (n - 1)
      
let rec print_term term indent =
  match term with
  | Program p -> print_spaces indent; print_string "Program\n"; print_term p (indent+2)
  | Clause c -> print_spaces indent; print_string "Clause\n"; print_term c (indent+2)
  | Fact a -> print_spaces indent; print_string "Fact\n"; print_term a (indent+2)
  | Rule (a, b) -> print_spaces indent; print_string "Rule\n"; print_term a (indent+2); print_term b (indent+2)
  | Termlist l -> List.iter (fun t -> print_term t indent) l
  | Head h -> print_spaces indent; print_string "Head\n"; print_term h (indent+2)
  | Body b -> print_spaces indent; print_string "Body\n"; print_term b (indent+2)
  | Goal g -> print_spaces indent; print_string "Goal\n"; print_term g (indent+2)
  | Int i -> print_spaces indent; print_string "Int: "; print_int i; print_string "\n"
  | String s -> print_spaces indent; print_string "String: "; print_string s; print_string "\n"
  | Variable v -> print_spaces indent; print_string "Variable: "; print_string v; print_string "\n"
  | Keyword k -> print_spaces indent; print_string "Keyword: "; print_string k; print_string "\n"
  | Atom a -> print_spaces indent; print_string "Atom: "; print_string a; print_string "\n"
  | Bool b -> print_spaces indent; print_string "Bool: "; print_string (string_of_bool b); print_string "\n"
  | Op (a, op, b) ->
      print_spaces indent;
      print_string "Operator Statement:\n";
      print_spaces (indent+2);
      print_string "Variable: ";
      print_string a; 
      print_string "\n";
      print_spaces (indent+2);          
      print_string "Operator: "; 
      print_string op; 
      print_string "\n";   
      print_spaces (indent+2);       
      print_string "Term:\n"; 
      print_term b (indent+4);
  | Atomicformula (atom, terms) -> 
    print_spaces indent;
    print_string "Atomic Formula:\n";            
    print_spaces (indent+2);
    print_string "Functor: "; 
    print_string atom; 
    print_string "\n";           
    print_spaces (indent+2);
    print_string "Argument Terms:\n"; 
    print_term terms (indent+4)
  | Funct (atom, terms) ->
      print_spaces indent;
      print_string "Function:\n";            
      print_spaces (indent+2);
      print_string "Function Name: "; 
      print_string atom; 
      print_string "\n";            
      print_spaces (indent+2);
      print_string "Arguments:\n"; 
      print_term terms (indent+4)
  ;;  

let main =
  let inp = open_in "input.txt" in
  let lexbuf = Lexing.from_channel inp in
  let res =
  try
    Parser.main Lexer.token lexbuf
  with 
  | Lexer.Error msg ->
    fprintf stderr "Lexical error at line '%d': Character '%c'\n" 
    lexbuf.lex_curr_p.pos_lnum msg;
    exit 1
  | Stdlib.Parsing.Parse_error ->
    fprintf stderr "Parser error at line '%d'\n" lexbuf.lex_curr_p.pos_lnum;
    exit 1
  in
  print_term res 0
;;

(* 

harry(lily, false) :- james(Snape, Tom), sirius(ron(2), Hermione), Voldemort is 2.
frodo(Bilbo).
darth(vader) :- lightsaber(true).
meal(X) :- food(23).
popcorn(Movie) :- !.
study(_) :- fail.
x(a(X), b(X), c(d(X)), e(Y)) :- N is M+2, N1 is M1/3.
?- a(b, c), S is "hello, world!".

*)
