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

let rec krivine (focus:clos) (stack:(clos list)) =
match focus with 
| Clos(V x, g) -> krivine (try List.assoc x g with Not_found -> failwith "Variable not in gamma") stack
| Clos(Abs(x, e), g) ->
    (
      match stack with
      | cl::s -> krivine (Clos(e, (x,cl)::g)) s
      | _ -> failwith "Argument not on stack"
    )
| Clos(App(e1, e2), g) -> krivine (Clos(e1, g)) (Clos(e2, g)::stack)
| _ -> focus, stack
;;

let rec unpack (focus:clos) : exp =
match focus with
| Clos(N n, g) -> N n
| Clos(B b, g) -> B b
| Clos(V x, g) -> (try unpack (List.assoc x g) with Not_found -> failwith "Variable not in gamma")
| Clos(Add(e1, e2), g) -> Add((unpack (Clos(e1, g))), (unpack (Clos(e2, g))))
| Clos(Sub(e1, e2), g) -> Sub((unpack (Clos(e1, g))), (unpack (Clos(e2, g))))
| Clos(Mul(e1, e2), g) -> Mul((unpack (Clos(e1, g))), (unpack (Clos(e2, g))))
| Clos(Div(e1, e2), g) -> Div((unpack (Clos(e1, g))), (unpack (Clos(e2, g))))
| Clos(Eq(e1, e2), g) -> Eq((unpack (Clos(e1, g))), (unpack (Clos(e2, g))))
| Clos(Gt(e1, e2), g) -> Gt((unpack (Clos(e1, g))), (unpack (Clos(e2, g))))
| Clos(Lt(e1, e2), g) -> Lt((unpack (Clos(e1, g))), (unpack (Clos(e2, g))))
| Clos(And(e1, e2), g) -> And((unpack (Clos(e1, g))), (unpack (Clos(e2, g))))
| Clos(Or(e1, e2), g) -> Or((unpack (Clos(e1, g))), (unpack (Clos(e2, g))))
| Clos(Not(e), g) -> Not((unpack (Clos(e, g))))
| Clos(If(e0, e1, e2), g) -> If((unpack (Clos(e0, g))), (unpack (Clos(e1, g))), (unpack (Clos(e2, g))))
| Clos(Tuple(l, n), g) -> Tuple((List.map (fun e -> unpack (Clos(e, g))) l), n)
| Clos(Proj(e, n), g) -> Proj(unpack (Clos(e, g)), n)
| Clos(Case(e, l), g) -> Case(unpack (Clos(e, g)), (List.map (fun (a, b) -> (unpack (Clos(a, g)), unpack (Clos(b, g)))) l))
| _ -> failwith "invalid closure to unpack"
;;

let rec eval (e:exp) : exp =
match e with
| N n -> N n
| B b -> B b
| Add(e1, e2) -> 
    (
      match eval(e1), eval(e2) with
      | N n1, N n2 -> N (n1+n2)
      | _ -> failwith "Type error"
    )
| Sub(e1, e2) -> 
    (
      match eval(e1), eval(e2) with
      | N n1, N n2 -> N (n1-n2)
      | _ -> failwith "Type error"
    )
| Mul(e1, e2) ->
    (
      match eval(e1), eval(e2) with
      | N n1, N n2 -> N (n1*n2)
      | _ -> failwith "Type error"
    )
| Div(e1, e2) ->
    (
      match eval(e1), eval(e2) with
      | N n1, N n2 -> if n2=0 then failwith "Division by Zero" else N (n1/n2)
      | _ -> failwith "Type error"
    )
| Eq(e1, e2) ->
    (
      match eval(e1), eval(e2) with
      | N n1, N n2 -> B (n1=n2)
      | _ -> failwith "Type error"
    )
| Gt(e1, e2) ->
    (
      match eval(e1), eval(e2) with
      | N n1, N n2 -> B (n1>n2)
      | _ -> failwith "Type error"
    )
| Lt(e1, e2) ->
    (
      match eval(e1), eval(e2) with
      | N n1, N n2 -> B (n1<n2)
      | _ -> failwith "Type error"
    )
| And(e1, e2) ->
    (
      match eval(e1), eval(e2) with
      | B b1, B b2 -> B (b1 && b2)
      | _ -> failwith "Type error"
    )
| Or(e1, e2) ->
    (
      match eval(e1), eval(e2) with
      | B b1, B b2 -> B (b1 || b2)
      | _ -> failwith "Type error"
    )
| Not(e) ->
    (
      match eval(e) with
      | B b -> B (not b)
      | _ -> failwith "Type error"
    )
| If(e0, e1, e2) ->
    (
      match eval(e0) with
      | B true -> eval(e1)
      | B false -> eval(e2)
      | _ -> failwith "Type error"
    )
| Tuple(l, n) -> Tuple ((List.map eval l), n)
| Proj(e, n) ->
    (
      match e with
      | Tuple(l, m) ->
          (
            if n <= 0 || n > m then
              failwith "Invalid projection index"
            else eval (List.nth l (n-1))
          )
      | _ -> failwith "Type error"
    )
| Case(e, l) -> eval(try List.assoc (eval e) l with Not_found -> failwith "Invalid expression to be matched")
| _ -> failwith "Invalid exp"
;;

let cbn (e:exp) :exp =
match krivine (formclos e) [] with
| cl, _ -> eval (unpack cl)
;; 

let get_focus c =
  match c with
  | c, _ -> c

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
  | _ -> ()
;;

let rec print_clos (indent:int) (c:clos) =
  match c with
  | Clos(e, g) -> print_spaces indent; print_string "Closure:\n"; 
                  print_spaces (indent+2); print_string "Expression:\n"; print_exp (indent+2) e;
                  print_spaces (indent+2); print_string "Gamma:\n";
                  List.iter (fun (x, cl) -> print_spaces (indent+4); print_string x; print_string "->\n"; print_clos (indent+6) cl) g

let exp1 = App(Abs("x", N 3), Div(N 2, N 0));;
print_clos 0 (formclos exp1);; 
print_clos 0 (get_focus (krivine (formclos exp1) []));;
print_exp 0 (cbn exp1) ;;

let exp2 = Proj(Tuple([Tuple([], 0); (N 1)], 2), 1);;
print_clos 0 (formclos exp2);;
print_clos 0 (get_focus (krivine (formclos exp2) []));;
print_exp 0 (cbn exp2) ;;

let exp3 = App(App(Abs("x", Abs("y", (Add(V "y", N 2)))), Div(N 4, N 0)), N 5);;
print_clos 0 (formclos exp1);;
print_clos 0 (get_focus (krivine (formclos exp3) []));;
print_exp 0 (cbn exp3) ;;