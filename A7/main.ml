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
and clos = Clos of (exp * table)
and table = (string * clos) list
;;

let formclos (e:exp) :clos = Clos(e,[])
;;

let get_focus c =
  match c with
  | c, _ -> c

let rec krivine (focus:clos) (stack:(clos list)) : (clos * (clos list)) =
match focus with 
| Clos(V x, g) -> krivine (try List.assoc x g with Not_found -> failwith "Variable not in gamma") stack
| Clos(Abs(x, e), g) ->
    (
      match stack with
      | cl::s -> krivine (Clos(e, (x,cl)::g)) s
      | [] -> Clos(Abs(x, e), g), stack
    )
| Clos(App(e1, e2), g) -> krivine (Clos(e1, g)) (Clos(e2, g)::stack)
| Clos(Add(e1, e2), g) -> 
  (
    match get_focus (krivine (Clos(e1, g)) stack), get_focus (krivine (Clos(e2, g)) stack) with
    | Clos(N n1, _), Clos(N n2, _) -> Clos(N (n1+n2), g), stack
    | _ -> failwith "Type error"
  )
| Clos(Sub(e1, e2), g) -> 
  (
    match get_focus (krivine (Clos(e1, g)) stack), get_focus (krivine (Clos(e2, g)) stack) with
    | Clos(N n1, _), Clos(N n2, _) -> Clos(N (n1-n2), g), stack
    | _ -> failwith "Type error"
  )
| Clos(Mul(e1, e2), g) -> 
  (
    match get_focus (krivine (Clos(e1, g)) stack), get_focus (krivine (Clos(e2, g)) stack) with
    | Clos(N n1, _), Clos(N n2, _) -> Clos(N (n1*n2), g), stack
    | _ -> failwith "Type error"
  )
| Clos(Div(e1, e2), g) -> 
  (
    match get_focus (krivine (Clos(e1, g)) stack), get_focus (krivine (Clos(e2, g)) stack) with
    | Clos(N n1, _), Clos(N n2, _) -> if n2=0 then failwith "Division by Zero" else Clos(N (n1/n2), g), stack
    | _ -> failwith "Type error"
  )
| Clos(Eq(e1, e2), g) -> 
  (
    match get_focus (krivine (Clos(e1, g)) stack), get_focus (krivine (Clos(e2, g)) stack) with
    | Clos(N n1, _), Clos(N n2, _) -> Clos(B (n1=n2), g), stack
    | _ -> failwith "Type error"
  )
| Clos(Gt(e1, e2), g) -> 
  (
    match get_focus (krivine (Clos(e1, g)) stack), get_focus (krivine (Clos(e2, g)) stack) with
    | Clos(N n1, _), Clos(N n2, _) -> Clos(B (n1>n2), g), stack
    | _ -> failwith "Type error"
  )
| Clos(Lt(e1, e2), g) -> 
  (
    match get_focus (krivine (Clos(e1, g)) stack), get_focus (krivine (Clos(e2, g)) stack) with
    | Clos(N n1, _), Clos(N n2, _) -> Clos(B (n1<n2), g), stack
    | _ -> failwith "Type error"
  )
| Clos(And(e1, e2), g) -> 
  (
    match get_focus (krivine (Clos(e1, g)) stack), get_focus (krivine (Clos(e2, g)) stack) with
    | Clos(B b1, _), Clos(B b2, _) -> Clos(B (b1 && b2), g), stack
    | _ -> failwith "Type error"
  )
| Clos(Or(e1, e2), g) -> 
  (
    match get_focus (krivine (Clos(e1, g)) stack), get_focus (krivine (Clos(e2, g)) stack) with
    | Clos(B b1, _), Clos(B b2, _) -> Clos(B (b1 || b2), g), stack
    | _ -> failwith "Type error"
  )
| Clos(Not(e), g) -> 
  (
    match get_focus (krivine (Clos(e, g)) stack) with
    | Clos(B b, _) -> Clos(B (not b), g), stack
    | _ -> failwith "Type error"
  )
| Clos(If(e0, e1, e2), g) -> 
  (
    match get_focus (krivine (Clos(e0, g)) stack) with
    | Clos(B true, _) -> krivine (Clos(e1, g)) stack
    | Clos(B false, _) -> krivine (Clos(e2, g)) stack
    | _ -> failwith "Type error"
  )
| Clos(Tuple(l, n), g) -> 
  (
    let rec eval_tuple (l:exp list) (g:table) (stack:(clos list)) :exp list =
      match l with
      | [] -> []
      | e::l1 -> 
        (
          match get_focus (krivine (Clos(e, g)) stack) with
          | Clos(e', _) -> e'::(eval_tuple l1 g stack)
        )
    in
    Clos(Tuple((eval_tuple l g stack), n), g), stack
  )
| Clos(Proj(e, n), g) -> 
  (
    match get_focus (krivine (Clos(e, g)) stack) with
    | Clos(Tuple(l, m), _) -> 
        (
          if n <= 0 || n > m then
            failwith "Invalid projection index"
          else Clos(List.nth l (n-1), g), stack
        )
    | _ -> failwith "Type error"
  )
| Clos(Case(e, l), g) ->
  (
    let l1 = List.map (fun (a, b) -> (match get_focus(krivine (Clos(a, g)) stack) with | Clos(e', _) -> e', b)) l in
    match get_focus (krivine (Clos(e, g)) stack) with
    | Clos(e', _) -> try krivine (Clos(List.assoc e' l1, g)) stack with Not_found -> failwith "Invalid expression to be matched"
    | _ -> failwith "Type error"
  )
| _ -> focus, stack
;;

let rec unpack (focus:clos) : exp =
  match focus with
  | Clos(e, g) -> e
;;

let cbn (e:exp) :exp = unpack (get_focus (krivine (formclos e) []))
;;

let rec print_spaces (n:int) =
  if n > 0 then (print_string " "; print_spaces (n-1))
  else ()
;;

let rec print_exp (indent:int) (e:exp) =
  match e with
  | N n -> print_spaces indent; print_string "Numeral "; print_int n; print_newline()
  | B b -> print_spaces indent; print_string "Bool "; print_string (string_of_bool b); print_newline()
  | V x -> print_spaces indent; print_string "Variable ";print_string x; print_newline()
  | Add(e1, e2) -> print_spaces indent; print_string "Add:\n"; print_exp (indent+2) e1; print_exp (indent+2) e2
  | Sub(e1, e2) -> print_spaces indent; print_string "Sub:\n"; print_exp (indent+2) e1; print_exp (indent+2) e2
  | Mul(e1, e2) -> print_spaces indent; print_string "Mul:\n"; print_exp (indent+2) e1; print_exp (indent+2) e2
  | Div(e1, e2) -> print_spaces indent; print_string "Div:\n"; print_exp (indent+2) e1; print_exp (indent+2) e2
  | Eq(e1, e2) -> print_spaces indent; print_string "Eq:\n"; print_exp (indent+2) e1; print_exp (indent+2) e2
  | Gt(e1, e2) -> print_spaces indent; print_string "Gt:\n"; print_exp (indent+2) e1; print_exp (indent+2) e2
  | Lt(e1, e2) -> print_spaces indent; print_string "Lt:\n"; print_exp (indent+2) e1; print_exp (indent+2) e2
  | And(e1, e2) -> print_spaces indent; print_string "And:\n"; print_exp (indent+2) e1; print_exp (indent+2) e2
  | Or(e1, e2) -> print_spaces indent; print_string "Or:\n"; print_exp (indent+2) e1; print_exp (indent+2) e2
  | Not(e) -> print_spaces indent; print_string "Not:\n"; print_exp (indent+2) e
  | If(e0, e1, e2) -> print_spaces indent; print_string "If:\n"; 
                     print_spaces (indent+2); print_string "Condition:\n"; print_exp (indent+4) e0;
                     print_spaces (indent+2); print_string "Exp1:\n"; print_exp (indent+4) e1;
                     print_spaces (indent+2); print_string "Exp2:\n"; print_exp (indent+4) e2
  | Tuple(l, n) -> print_spaces indent; print_string "Tuple:\n"; 
                   print_spaces (indent+2); print_string "Size "; print_int n; print_newline();
                   List.iter (fun e -> print_exp (indent+2) e) l;
  | Proj(e, n) -> print_spaces indent; print_string "Proj:\n"; print_exp (indent+2) e; 
                  print_spaces (indent+2); print_string "Index "; print_int n; print_newline()
  | Case(e, l) -> print_spaces indent; print_string "Case:\n";
                  print_spaces (indent+2); print_string "Match with:\n"; print_exp (indent+2) e;
                  List.iter (fun (a, b) -> print_spaces (indent+2); print_string "C:\n"; print_exp (indent+4) a; 
                  print_spaces (indent+2); print_string "E:\n"; print_exp (indent+4) b) l
  | Abs(x, e) -> print_spaces indent; print_string "Abstraction:\n"; print_spaces indent; print_string "Parameter "; print_string x; print_newline();
                 print_spaces indent; print_string "Body:\n"; print_exp (indent+2) e
  | App(e1, e2) -> print_spaces indent; print_string "Application:\n"; print_exp (indent+2) e1; print_exp (indent+2) e2
;;

let rec print_clos (indent:int) (c:clos) =
  match c with
  | Clos(e, g) -> print_spaces indent; print_string "Closure:\n"; 
                  print_spaces (indent+2); print_string "Expression:\n"; print_exp (indent+4) e;
                  print_spaces (indent+2); print_string "Gamma:\n";
                  if List.length g = 0 then (print_spaces (indent+4); print_string "Empty\n")
                  else List.iter (fun (x, cl) -> print_spaces (indent+4); print_string x; print_string "->\n"; print_clos (indent+6) cl) g
;;

(* let exp1 = App(Abs("x", N 3), Div(N 2, N 0));;
print_exp 0 (cbn exp1) ;; *)

(* let exp2 = Proj(Tuple([Tuple([], 0); (N 1)], 2), 1);;
print_exp 0 (cbn exp2) ;; *)

(* let exp3 = App(App(Abs("x", Abs("y", (Add(V "y", N 2)))), Div(N 4, N 0)), N 5);;
print_exp 0 (cbn exp3) ;; *)

(* let exp4 = App(Abs("x", Mul(V "x", N 0)), Div(N 7, N 0));;
print_exp 0 (cbn exp4) ;; *)

(* let exp5 = Abs("x", Mul(V "x", N 0));;
print_exp 0 (cbn exp5) ;; *)

(* let exp6 = V "x";;
print_clos 0 (get_focus (krivine (Clos(exp6, [("x", Clos(App(Abs("y", N 3), Div(N 2, N 0)), []))])) []));; *)

(* let exp7 = Add(App(Abs("x", Add(V "x", N 3)), N 2), N 5);;
print_exp 0 (cbn exp7) ;; *)

(* let exp8 = Abs("x", Abs("y", Mul(V "x", V "y")));;
print_exp 0 (cbn exp8) ;; *)
