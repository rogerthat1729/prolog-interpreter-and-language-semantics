open Printf
open Types

(*
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

let change_goal_tree (t:termtype) :termtype list =
  match t with
  | Program (Termlist l) -> l
  | _ -> failwith "Enter a valid goal"
;;

let rec give_vars t =
  match t with
  | Int _ | String _ | Atom _ | Keyword _ | Bool _ -> []
  | Variable v -> [v]
  | Program p | Clause p | Fact p | Head p | Body p | Goal p -> give_vars p
  | Rule (a, b) -> (give_vars a) @ (give_vars b)
  | Termlist l -> List.fold_left (fun acc x -> acc @ (give_vars x)) [] l
  | Op (a, _, b) -> a :: (give_vars b)
  | Atomicformula (_, b) | Funct (_, b) -> give_vars b
*)

let rec print_term (t:termtype) = 
  match t with 
  | Vector (s, l) -> 
    (
      match s with 
      | "_LST" -> print_string "["; (List.iter 
      (fun t -> match t with 
      | Variable x -> if String.contains x '@' then print_string "_" else print_string x
      | _ -> print_term t; print_string "|"
      ) l); print_string "]";
      | _ -> print_string "Atomic Formula:\n"; print_endline s; print_string "Arguments:\n";
      (List.iter (print_term) l) 
    )
  | Variable x -> print_string "Var "; print_string x
  | Atom a -> print_string "Atom "; print_string a;
  | Int x -> print_string "Int "; print_string (string_of_int x);
  | String x -> print_string "String "; print_string x;
  | Bool x -> print_string "Bool "; print_string (string_of_bool x);
  | Nil -> print_endline "Empty"
  | Fail -> print_endline "Fail"
  | _ -> ()
;;

let print_table table =
  print_string "\nTable:\n";
  List.iter (fun (x, t) -> print_string x; print_string " = "; print_term t) table;
  print_endline ""
;;

(* let rec check_unifiable (t1:termtype) (t2:termtype) =
  match t1, t2 with
  | Int x, Int y -> if x = y then true else false
  | String x, String y -> if x = y then true else false
  | Atom x, Atom y -> if x = y then true else false
  | Keyword x, Keyword y -> if x = y then true else false
  | Bool x, Bool y -> if x = y then true else false
  | Variable x, Variable y -> true
  | Variable x, t | t, Variable x -> true
  | Atomicformula (a1, b1), Atomicformula (a2, b2) -> 
    (check_unifiable a1 a2) && (List.fold_left (&&) true (List.map2 check_unifiable b1 b2))
  | _ -> false *)

let rec union l1 l2 =
  match l1 with
  | [] -> l2
  | h::t -> if List.mem h l2 then union t l2 else h::(union t l2)
;;

let rec subst s t =
  match t with
  | Variable x -> s (Variable x)
  | Program prog -> Program (List.map (subst s) prog)
  | Rule (r, l) -> Rule ((subst s r), (List.map (subst s) l))
  | Clause c | Fact c -> subst s c
  | Vector(st, l) -> Vector(st, (List.map (subst s) l))
  | _ -> t
;;

let subst_helper sigma x =
  match x with
  | Variable x -> (try List.assoc x sigma with Not_found -> Variable x)
  | _ -> x
;;

let compose sigma1 sigma2 = 
  let all_vars = union (List.map (fun (a, b) -> a) sigma1) (List.map (fun (a, b) -> a) sigma2) in
  List.map (fun x -> (x, subst (subst_helper sigma2) (subst (subst_helper sigma1) (Variable x)))) all_vars
;;

let rec check_var_in_tree x t =
  match t with 
  | Variable y -> (x = y)
  | Program p | Goal p -> (List.fold_left (||) (false) (List.map (fun tree -> (check_var_in_tree x tree)) p))
  | Clause c | Fact c -> check_var_in_tree x c
  | Rule (r, l) -> (check_var_in_tree x r) || (List.fold_left (||) (false) (List.map (fun tree -> (check_var_in_tree x tree)) l))
  | Vector (s, l) -> (List.fold_left (||) (false) (List.map (fun tree -> (check_var_in_tree x tree)) l))
  | _ -> false

let rec unify_atomic_formulae (t1:termtype) (t2:termtype) :(string * termtype) list = 
  match t1, t2 with
  | Fail, _ | _, Fail -> failwith "NOT_UNIFIABLE"
  | Nil, Nil -> []
  | Int x, Int y -> if x = y then [] else failwith "NOT_UNIFIABLE"
  | String x, String y -> if x = y then [] else failwith "NOT_UNIFIABLE"
  | Atom x, Atom y -> if x = y then [] else failwith "NOT_UNIFIABLE"
  | Bool x, Bool y -> if x = y then [] else failwith "NOT_UNIFIABLE"
  | Variable x, Variable y -> if x = y then [] else [x, Variable y]
  | Variable x, t | t, Variable x ->
    if String.contains x '@' then []
    else if check_var_in_tree x t then failwith "NOT_UNIFIABLE"
    else [x, t]
  | Vector(s1, l1), Vector(s2, l2) ->
    if s1 <> s2 || (List.length l1) <> (List.length l2) then failwith "NOT_UNIFIABLE"
    else
      (
        (* print_endline("here"); *)
        let rec iterate idx ls1 ls2 =
          match idx with
          | 0 -> []
          | _ -> 
            (* print_endline "here";
            print_int idx; print_endline "";
            print_term (List.nth ls1 (idx-1));
            print_term (List.nth ls2 (idx-1)); *)
            let prev = iterate (idx-1) ls1 ls2 in
            (* print_table prev; *)
            let newt1 = subst (subst_helper prev) (List.nth ls1 (idx-1)) in
            let newt2 = subst (subst_helper prev) (List.nth ls2 (idx-1)) in
            (* print_term newt1;
            print_term newt2;
            print_table (unify_atomic_formulae newt1 newt2); *)
            compose prev (unify_atomic_formulae newt1 newt2)
          in
        iterate (List.length l1) l1 l2
      ) 
  | _ -> failwith "NOT_UNIFIABLE"
;;

let apply_sigma sigma (t:termtype) = 
  match t with 
  | Variable x -> (try List.assoc x sigma with Not_found -> Variable x)
  | _ -> t
;;

let convert_program_to_list (p:termtype) : ((termtype * (termtype list)) list) = 
  match p with 
  | Program l ->
    List.map (fun t -> 
      match t with 
      | Clause (Fact f) -> f, []
      | Clause (Rule (r1, l1)) -> r1, l1 
      | _ -> failwith "Not a clause") l
  | _ -> failwith "Not a program"
    ;;

let rec change_program_variables p :termtype = 
  match p with
  | Variable x -> Variable ("_"^x)
  | Vector (s, l) -> Vector (s, (List.map change_program_variables l))
  | _ -> p
;;

let rec change_variables_for_list lst = List.map (fun (r, l) -> ((change_program_variables r), (List.map change_program_variables l))) lst
;;

let convert_goal_to_list (g:termtype) = 
  match g with 
  | Program l -> 
    (List.flatten (List.map (fun t -> match t with Goal goal -> goal | _ -> failwith "Not a goal") l))
  | _ -> failwith "Not a goal"
  ;;

(* let rec give_vars t =
  match t with 
  | Variable x -> [x]
  | Atomicformula (Atom a, l) -> List.flatten (List.map give_vars l)
  | List (hd, tl) -> (give_vars hd) @ (give_vars tl)
  | _ -> [] *)

(* let rec af_equal t1 t2 = 
  match t1, t2 with
  | Atomicformula (a1, l1), Atomicformula (a2, l2) ->
    if a1 = a2 then List.fold_left (&&) true (List.map2 (fun x y -> af_equal x y) l1 l2)
    else false
  | List(hd1, tl1), List(hd2, tl2) -> (af_equal hd1 hd2) && (af_equal tl1 tl2)
  | Int x, Int y -> if x = y then true else false
  | String x, String y -> if x = y then true else false
  | Atom x, Atom y -> if x = y then true else false
  | Bool x, Bool y -> if x = y then true else false
  | _, _ -> false *)

(* let check_var_in_af x t = 
  let vars = give_vars t in
  List.mem x vars *)

(* let check_var_in_list x t =
  let vars = give_vars t in
  List.mem x vars *)

let rec solve_goal s (goal:termtype list) (prog) :(bool * ((string * termtype) list)) =
  match goal with
  | [] -> true, s
  | g :: gs -> 
    (
      let newprog = change_variables_for_list prog in
      let rec run_on_prog g prg =
        match prg with 
        | (r, l) :: ps ->
          (* print_table s;
          print_term r;
          print_term g; *)
          (
            try
              let subs = unify_atomic_formulae g r in
              (* print_endline "In try block";
              print_table subs; *)
              let newgoal = List.map (subst (subst_helper subs)) (l@gs) in
              let newsubs = compose s subs in
              (* print_table newsubs; *)
              (* print_int (List.length newgoal); print_endline ""; *)
              let ans, finalsubs = solve_goal newsubs newgoal newprog in
              (* print_endline "Final sub";
              print_table finalsubs; *)
              if ans then true, finalsubs
              else run_on_prog g ps
            with 
            | Failure msg -> run_on_prog g ps
          )
        | _ -> false, []
            in
        run_on_prog g prog
    )
      ;;

let apply_on_input buf = 
  try
    Parser.main Lexer.token buf
  with 
  | Lexer.Error msg ->
    fprintf stderr "Lexical error at line '%d': Character '%c'\n" 
    buf.lex_curr_p.pos_lnum msg;
    exit 1
  | Stdlib.Parsing.Parse_error ->
    fprintf stderr "Parser error at line '%d'\n" buf.lex_curr_p.pos_lnum;
    exit 1
  ;;

let print_ans (b:bool) s = 
  match b with 
  | true -> 
    (
      let goal_vars = List.filter (fun (x, t) -> not (String.contains x '_')) s in
      (* print_table goal_vars; *)
      if List.length goal_vars = 0 then print_string "true.\n"
      else List.iter (fun (x, t) -> print_string x; print_string " = "; print_term t) goal_vars
    )
  | false -> print_endline "false"
    ;;

let main =
  let inp = open_in "input.txt" in
  let goal = open_in "goal.txt" in
  let lexbuf = Lexing.from_channel inp in
  let goalbuf = Lexing.from_channel goal in
  let program_tree = apply_on_input lexbuf in
  let goal_tree = apply_on_input goalbuf in
  (* compose [("X", Variable "Y"); ("Z", Vector("mem", [Variable "X"]))] ["Y", Int 1] *)
  (* let modified_program = change_program_variables program_tree in *)
  let p = change_variables_for_list (convert_program_to_list program_tree) in
  let g = convert_goal_to_list goal_tree in
  (* fst (List.nth p 1) *)
  (* List.nth g 0 *)
    (* print_term (List.nth g 0);
  print_term (fst (List.nth p 1)); *)
  (* fst (List.nth p 1) *)

  (* unify_atomic_formulae (Variable "X") (Vector ("_LST", [Int 2; Variable "_@"])) *)
  (* unify_atomic_formulae (List.nth g 0) (fst (List.nth p 1)) *)
  (* unify_atomic_formulae (List.nth g 0) (fst (List.nth p 0)) *)
  (* solve_goal [] g p *)
  let b,s = solve_goal [] g p in
  print_ans b s
  (* check_program (List.nth g 0) p *)
  (* check_unifiable (Atomicformula (Atom "mother", [Variable "X"; Variable "Y"])) (Atomicformula (Atom "mother", [Variable "A"; Variable "B"])) *)
  (* List.map (fun gl -> check_program gl p) g *)
  (* print_term program_tree 0 *)
  (* let goal_tree = (Parser.main Lexer.token goalbuf) in
  List.iter (fun g -> print_term g 0) (change_goal_tree goal_tree); *)
  (* print_term (change_goal_tree program_tree) 0; *)
  (* List.iter (fun (a, b) -> print_endline a; print_term b 0) (unify (change_goal_tree goal_tree) (change_goal_tree program_tree)) *)
  (* print_term res 0 *)
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
