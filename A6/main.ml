(* To add - definitions(if possible)*)
(* Can make a lexer and parser for better quality *)

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
| Tuple of (exp list)*int
| Proj of exp*int
| Case of exp*((exp*exp) list)
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
| TUPLE of int
| PROJ of int
| CASE of ((opcode list)*(opcode list)) list
;;

type ans =
| N of int
| B of bool
| T of ans list
| VClos of string*(opcode list)*((string*ans) list)
;;

type table = (string*ans) list
;;

type stack = ans list
;;

type dump = (stack*(table)*(opcode list)) list
;;

let rec compile e =
match e with
| V x -> [LOOKUP x]
| Abs (x, e1) -> [MKCLOS (x, (compile e1)@[RET])]
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
| Not e1 -> (compile e1)@[NOT]
| If (e1, e2, e3) -> (compile e1)@[IF(compile e2, compile e3)]
| Tuple (l, n) -> 
(
  if n > 0 then (List.fold_left (@) [] (List.map (compile) l))@[TUPLE n]
  else failwith "Compilation Error"
)
| Proj (e1, n) -> compile(e1)@[PROJ n]
| Case (e, l) -> (compile e)@[CASE (List.map (fun (a, b)->(compile a, compile b)) l)]
;;

exception SegFault of stack*table*(opcode list)*dump
;;
exception ExecutionError of stack*table*(opcode list)*dump
;;
exception DivbyZero of stack*table*(opcode list)*dump
;;


let rec execute stk gamma comp dmp =
  match stk, gamma, comp, dmp with
  | x::xs, _, [], [] -> x
  | a::s1, _, RET::_, (s, g, c)::d -> execute (a::s) g c d
  | s, g, (LOOKUP x)::c, d -> execute ((try List.assoc x g with Not_found -> raise (SegFault (stk, gamma, comp, dmp)))::s) g c d
  | s, g, (MKCLOS (x, c1))::c2, d -> execute ((VClos (x,c1,g))::s) g c2 d
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
  | (B b)::s, g, (IF(c1, c2))::c, d -> 
    (
      match b with
      | true -> execute s g (c1@c) d
      | false -> execute s g (c2@c) d
    )
  | s, g, (TUPLE n)::c, d -> 
    (
      if List.length s < n then raise (ExecutionError (stk, gamma, comp, dmp))
      else
      (
        let rec split_at n lst =
          match n, lst with
          | _, [] -> ([], [])
          | 0, _ -> ([], lst)
          | n, x :: xs ->
              let (left, right) = split_at (n-1) xs in
              (x :: left, right)
          in
        let s1, s2 = split_at n s in
        execute ((T (List.rev s1))::s2) g c d
      )
    )
  | (T l)::s, g, (PROJ n)::c, d -> 
    (
      if (n > List.length l) || (n <= 0) then
        failwith "Invalid projection index"
      else
        execute ((List.nth l (n-1))::s) g c d
    )
  | a1::s, g, (CASE l)::c, d -> 
    (
      let calculated_list = List.map (fun (a, b) -> ((execute [] g a []), b)) l in
      let correct_opcodes = try List.assoc a1 calculated_list with Not_found -> failwith "No matching value" in
      execute s g (correct_opcodes@c) d
    )
  | _, _, _, _ -> raise (ExecutionError (stk, gamma, comp, dmp))
;;

let rec print_opcode_list l = 
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
  | (TUPLE n)::t -> Printf.printf "TUPLE with length '%d'\n" n;
  | (PROJ n)::t -> Printf.printf "PROJECTION with index '%d\n" n;
  | (CASE l)::t -> print_string "CASE:\nBegin\n"; List.iter (fun (a, b)-> print_opcode_list a; print_opcode_list b) l; print_string "End\n"
;;

let rec print_ans a =
  match a with
  | N n -> print_int n; print_string "\n"
  | B b -> print_string (string_of_bool b); print_string "\n";
  | T l -> print_string "Tuple:\nBegin\n"; List.iter (print_ans) l; print_string "End\n";
  | VClos (x, c, g) -> 
    (
      print_string "Closure:\nArgument:\n"; print_string x; print_string "\nOpcode List:\n"; print_opcode_list c;
      let rec print_table gamma =
        match gamma with
        | [] -> ()
        | (x, a)::t -> print_string x; print_string " ->\n"; print_ans a; print_table t
      in
      print_table g
    )
;;

let run test =
  let opcodes = compile test in
  print_ans (execute [] [] opcodes [])
;;

(* Test 1: Function application *)
(* run (App(Abs("x", Add(V("x"), N(2))), N(3)));; *)
(* Expected output: 5 *)

(* Test 2: Nested function application *)
(* run (App(Abs("x", App(Abs("y", Mul(V("x"), V("y"))), N(7))), N(5)));; *)
(* Expected output: 35 *)

(* Test 3: If-else statement *)
(* run (If(Gt(N(5), N(3)), N(1), N(0)));; *)
(* Expected output: 1 *)

(* Test 4: Tuple *)
(* run (Tuple([N(1); B(true); Eq(N(2), Div(N(3), N(3)))], 3));; *)
(* Expected output:  
Tuple:
Begin
1
true
false
End
*)

(* Test 5: Proj *)
(* run (Proj(Tuple([N(1); B(true)], 2), 2));; *)
(* Expected output: true *)

(* Test 6: Closure *)
(* run (Abs("x", App(Abs("y", Mul(V("x"), V("y"))), N(7))));; *)
(* Expected output:
Closure:
Argument:
x
Opcode List:
MKCLOS y
LOOKUP x
LOOKUP y
MUL
RET
LDNUM 7
APP
RET
*)

(* Test 7: Nested closure *)
(* run (Abs("x", App(Abs("y", App(Abs("z", Mul(V("x"), Mul(V("y"), V("z")))), N(7))), N(5))));; *)
(* Expected output:
Closure:
Argument:
x
Opcode List:
MKCLOS y
MKCLOS z
LOOKUP x
LOOKUP y
LOOKUP z
MUL
MUL
RET
LDNUM 7
APP
RET
LDNUM 5
APP
RET
*)

(* Test 8: Case Statement *)
(* run (Case(App(Abs("x", Add(V("x"), N(2))), N(3)), [((N 7), (B true));((N 5), (B false))]));; *)
(* Expected output: false *)


(* Test 9: Error in Case Statement *)
(* run (Case(App(Abs("x", Add(V("x"), N(2))), N(4)), [((N 7), (B true));((N 5), (B false))]));; *)
(* Expected output: Error *)