type exp = 
| V of string 
| Abs of string*exp 
| App of exp*exp 
| N of int 
| B of bool 
| Add of exp*exp 
| Sub of exp*exp 
| Mul of exp*exp 
| Div of exp*exp 
| Eq of exp*exp 
| Gt of exp*exp 
| Lt of exp*exp 
| And of exp*exp 
| Or of exp*exp 
| Not of exp 
| If of exp*exp*exp
| Pair of exp*exp 
| Fst of exp 
| Snd of exp 
;;

type opcode = 
| LOOKUP of string 
| MKCLOS of string*(opcode list) 
| APP
| RET
| LDNUM of int
| LDBOOL of bool
| ADD
| SUB
| MUL
| DIV
| EQ
| GT
| LT
| AND
| OR
| NOT
| IF of (opcode list) * (opcode list)
| PAIR
| FST
| SND
;;

type ans =
| N of int
| B of bool
| P of ans*ans
| VClos of string*(opcode list)*((string*ans) list)
;;

type table = (string*ans) list
;;

type stack = ans list
;;

type dump = (stack*(table)*(opcode list)) list
;;

let rec compile (e:exp) =
match e with
| V x -> [LOOKUP x]
| Abs (x, e) -> [MKCLOS (x, (compile e)@[RET])]
| App (e1, e2) -> (compile e1)@(compile e2)@[APP]
| N n -> [LDNUM n]
| B b -> [LDBOOL b]
| Add (e1, e2) -> (compile e1)@(compile e2)@[ADD]
| Sub (e1, e2) -> (compile e1)@(compile e2)@[SUB]
| Mul (e1, e2) -> (compile e1)@(compile e2)@[MUL]
| Div (e1, e2) -> (compile e1)@(compile e2)@[DIV]
| Eq (e1, e2) -> (compile e1)@(compile e2)@[EQ]
| Gt (e1, e2) -> (compile e1)@(compile e2)@[GT]
| Lt (e1, e2) -> (compile e1)@(compile e2)@[LT]
| And (e1, e2) -> (compile e1)@(compile e2)@[AND]
| Or (e1, e2) -> (compile e1)@(compile e2)@[OR]
| Not e -> (compile e)@[NOT]
| If (e1, e2, e3) -> (compile e1)@[IF(compile e2, compile e3)]
| Pair (e1, e2) -> (compile e1)@(compile e2)@[PAIR]
| Fst e -> (compile e)@[FST]
| Snd e -> (compile e)@[SND]
;;

exception SegFault of stack*table*(opcode list)*dump
;;
exception CompilationError of stack*table*(opcode list)*dump
;;
exception DivbyZero of stack*table*(opcode list)*dump
;;

let rec execute (stk:stack) (gamma:table) (comp:opcode list) (dmp:dump) =
  match stk, gamma, comp, dmp with
  | x::xs, _, [], [] -> x
  | a::s1, _, RET::_, (s, g, c)::d -> execute (a::s) g c d
  | s, g, (LOOKUP x)::c, d -> execute ((try List.assoc x g with Not_found -> raise (SegFault (stk, gamma, comp, dmp)))::s) g c d
  | s, g, MKCLOS (x, c1)::c2, d -> execute ((VClos (x,c1,g))::s) g c2 d
  | a::(VClos (x,c1,g1))::s, g, APP::c2, d -> execute [] ((x, a)::g1) c1 ((s, g, c2)::d)
  | s, g, (LDNUM n)::c, d -> execute ((N n)::s) g c d
  | s, g, (LDBOOL b)::c, d -> execute ((B b)::s) g c d
  | (N n2)::(N n1)::s, g, ADD::c, d -> execute ((N (n1+n2))::s) g c d
  | (N n2)::(N n1)::s, g, SUB::c, d -> execute ((N (n1-n2))::s) g c d
  | (N n2)::(N n1)::s, g, MUL::c, d -> execute ((N (n1*n2))::s) g c d
  | (N n2)::(N n1)::s, g, DIV::c, d -> 
    (
      match n2 with
      | 0 -> raise (DivbyZero (stk, gamma, comp, dmp))
      | _ -> execute ((N (n1/n2))::s) g c d
    )
  | (N n2)::(N n1)::s, g, EQ::c, d -> execute ((B (n1=n2))::s) g c d
  | (N n2)::(N n1)::s, g, GT::c, d -> execute ((B (n1>n2))::s) g c d
  | (N n2)::(N n1)::s, g, LT::c, d -> execute ((B (n1<n2))::s) g c d
  | (B b2)::(B b1)::s, g, AND::c, d -> execute ((B (b1 && b2))::s) g c d
  | (B b2)::(B b1)::s, g, OR::c, d -> execute ((B (b1 || b2))::s) g c d
  | (B b)::s, g, NOT::c, d -> execute ((B (not b))::s) g c d
  | (B b)::s, g, IF(c1, c2)::c, d -> 
    (
      match b with
      | true -> execute s g (c1@c) d
      | false -> execute s g (c2@c) d
    )
  | a2::a1::s, g, PAIR::c, d -> execute ((P (a1, a2))::s) g c d
  | (P (a1, a2))::s, g, FST::c, d -> execute (a1::s) g c d
  | (P (a1, a2))::s, g, SND::c, d -> execute (a2::s) g c d
  | _, _, _, _ -> raise (CompilationError (stk, gamma, comp, dmp))
;;

let rec print_opcode_list (l:opcode list) = 
  match l with
  | [] -> ()
  | LOOKUP x::t -> print_string "LOOKUP "; print_string x; print_string "\n"; print_opcode_list t
  | MKCLOS (x, c)::t -> print_string "MKCLOS "; print_string x; print_string "\n"; print_opcode_list c; print_opcode_list t
  | APP::t -> print_string "APP\n"; print_opcode_list t
  | RET::t -> print_string "RET\n"; print_opcode_list t
  | LDNUM n::t -> print_string "LDNUM "; print_int n; print_string "\n"; print_opcode_list t
  | LDBOOL b::t -> print_string "LDBOOL "; print_string (string_of_bool b); print_string "\n"; print_opcode_list t
  | ADD::t -> print_string "ADD\n"; print_opcode_list t
  | SUB::t -> print_string "SUB\n"; print_opcode_list t
  | MUL::t -> print_string "MUL\n"; print_opcode_list t
  | DIV::t -> print_string "DIV\n"; print_opcode_list t
  | EQ::t -> print_string "EQ\n"; print_opcode_list t
  | GT::t -> print_string "GT\n"; print_opcode_list t
  | LT::t -> print_string "LT\n"; print_opcode_list t
  | AND::t -> print_string "AND\n"; print_opcode_list t
  | OR::t -> print_string "OR\n"; print_opcode_list t
  | NOT::t -> print_string "NOT\n"; print_opcode_list t
  | IF (c1, c2)::t -> print_string "IF:\n"; print_opcode_list c1; print_opcode_list c2; print_opcode_list t
  | PAIR::t -> print_string "PAIR\n"; print_opcode_list t
  | FST::t -> print_string "FST\n"; print_opcode_list t
  | SND::t -> print_string "SND\n"; print_opcode_list t

let rec print_ans (a:ans) =
  match a with
  | N n -> print_int n; print_string "\n"
  | B b -> print_string (string_of_bool b); print_string "\n";
  | P (a1, a2) -> print_string "Pair:\n"; print_ans a1; print_ans a2;
  | VClos (x, c, g) -> print_string "Closure:\nArgument:\n"; print_string x; print_string "\nOpcode List:\n"; print_opcode_list c; print_string "Table\n";
;;

(* Test 1: Simple function application *)
let test1 = 
  let expr = App(Abs("x", Add(V("x"), N(2))), N(3)) in
  let opcodes = compile expr in
  execute [] [] opcodes []
;;
(* Expected output: 5 *)

(* print_ans test1 ;; *)

(* Test 2: Nested function application *)

let test2 = 
  let expr = App(Abs("x", App(Abs("y", Mul(V("x"), V("y"))), N(7))), N(5)) in
  let opcodes = compile expr in
  execute [] [] opcodes []
;;
(* Expected output: 35 *)

(* print_ans test2 ;; *)

(* Test 3: If-else statement *)
let test3 =
  let expr = If(Gt(N(5), N(3)), N(1), N(0)) in
  let opcodes = compile expr in
  execute [] [] opcodes []
;;
(* Expected output: 1 *)

(* print_ans test3;; *)

(* Test 4: Pair *)
let test4 =
  let expr = Pair(N(1), B(true)) in
  let opcodes = compile expr in
  execute [] [] opcodes []
;;
(* Expected output:  
Pair:
1
true
*)

(* print_ans test4;; *)

(* Test 5: Fst *)
let test5 =
  let expr = Fst(Pair(N(1), B(true))) in
  let opcodes = compile expr in
  execute [] [] opcodes []
;;
(* Expected output: 1 *)
(* print_ans test5;; *)

(* Test 6: Snd *)
let test6 =
  let expr = Snd(Pair(N(1), B(true))) in
  let opcodes = compile expr in
  execute [] [] opcodes []
;;
(* Expected output: true *)
(* print_ans test6;; *)
