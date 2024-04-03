(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)

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
 
 type kconf = clos * (clos list)
 ;;
 
 
 let rec krivine (state:kconf):kconf = 
   match state with
   | Clos(V x, g), s -> (try krivine ((List.assoc x g), s) with Not_found -> failwith "Variable not found")
   | Clos(Abs(x, e), g), cl::s -> krivine (Clos(e, ((x, cl)::g)), s)
   | Clos(App(e1, e2), g), s -> krivine (Clos(e1, g), (Clos(e2, g)::s))
   | Clos(N n, g), s -> (Clos(N n, g), s)
   | Clos(B b, g), s -> (Clos(B b, g), s)
   | Clos(Add(e1, e2), g), s -> 
       (
         match (krivine (Clos(e1, g), s)), (krivine (Clos(e2, g), s)) with
         | (Clos(N n1, g1), s1), (Clos(N n2, g2), s2) -> (Clos(N (n1+n2), g), s)
         | _ -> failwith "Type error"
       )
   | Clos(Sub(e1, e2), g), s ->
       (
         match (krivine (Clos(e1, g), s)), (krivine (Clos(e2, g), s)) with
         | (Clos(N n1, g1), s1), (Clos(N n2, g2), s2) -> (Clos(N (n1-n2), g), s)
         | _ -> failwith "Type error"
       )
   | Clos(Mul(e1, e2), g), s ->
       (
         match (krivine (Clos(e1, g), s)), (krivine (Clos(e2, g), s)) with
         | (Clos(N n1, g1), s1), (Clos(N n2, g2), s2) -> (Clos(N (n1*n2), g), s)
         | _ -> failwith "Type error"
       )
   | Clos(Div(e1, e2), g), s ->
       (
         match (krivine (Clos(e1, g), s)), (krivine (Clos(e2, g), s)) with
         | (Clos(N n1, g1), s1), (Clos(N n2, g2), s2) -> if n2=0 then failwith "Division by Zero" else (Clos(N (n1/n2), g), s)
         | _ -> failwith "Type error"
       )
   | Clos(Eq(e1, e2), g), s ->
       (
         match (krivine (Clos(e1, g), s)), (krivine (Clos(e2, g), s)) with
         | (Clos(N n1, g1), s1), (Clos(N n2, g2), s2) -> (Clos(B (n1=n2), g), s)
         | _ -> failwith "Type error"
       )
   | Clos(Gt(e1, e2), g), s ->
       (
         match (krivine (Clos(e1, g), s)), (krivine (Clos(e2, g), s)) with
         | (Clos(N n1, g1), s1), (Clos(N n2, g2), s2) -> (Clos(B (n1>n2), g), s)
         | _ -> failwith "Type error"
       )
   | Clos(Lt(e1, e2), g), s ->
       (
         match (krivine (Clos(e1, g), s)), (krivine (Clos(e2, g), s)) with
         | (Clos(N n1, g1), s1), (Clos(N n2, g2), s2) -> (Clos(B (n1<n2), g), s)
         | _ -> failwith "Type error"
       )
   | Clos(And(e1, e2), g), s ->
       (
         match (krivine (Clos(e1, g), s)), (krivine (Clos(e2, g), s)) with
         | (Clos(B b1, g1), s1), (Clos(B b2, g2), s2) -> (Clos(B (b1 && b2), g), s)
         | _ -> failwith "Type error"
       )
   | Clos(Or(e1, e2), g), s ->
       (
         match (krivine (Clos(e1, g), s)), (krivine (Clos(e2, g), s)) with
         | (Clos(B b1, g1), s1), (Clos(B b2, g2), s2) -> (Clos(B (b1 || b2), g), s)
         | _ -> failwith "Type error"
       )
   | Clos(Not(e), g), s ->
       (
         match (krivine (Clos(e, g), s)) with
         | (Clos(B b, g1), s1) -> (Clos(B (not b), g), s)
         | _ -> failwith "Type error"
       )
   | Clos(If(e0, e1, e2), g), s ->
       (
         match (krivine (Clos(e0, g), [])) with
         | (Clos(B true, g1), s1) -> krivine (Clos(e1, g), s)
         | (Clos(B false, g1), s1) -> krivine (Clos(e2, g), s)
         | _ -> failwith "Type error"
       )
   | Clos(Tuple(l, n), g), s ->
       (
         let rec tuple_eval l g s n =
           match l with
           | [] -> (Clos(Tuple([], n), g), s)
           | e::l1 -> 
               (
                 match krivine (Clos(e, g), s) with
                 | (Clos(e1, g1), s1) -> 
                     (
                       match (tuple_eval l1 g1 s1 n) with
                       | (Clos(Tuple(l2, n), g2), s2) -> (Clos(Tuple(e1::l2, n), g2), s2)
                       | _ -> failwith "Type error"
                     )
               )
         in tuple_eval l g s n
       )
   | Clos(Proj(e, n), g), s ->
       (
         match e with 
         | Tuple(l, m) ->
             (
               if m < n then failwith "Invalid projection index"
               else krivine (Clos((List.nth l (n-1)), g), s)
             )
         | _ -> failwith "Type error"
       )
   | Clos(Case(e, l), g), s ->
       (
         match krivine (Clos(e, g), s) with
         | Clos(c, g), s -> try krivine (Clos(List.assoc c l, g), s) with Not_found -> failwith "Pattern matching failed"
                                                                        | _ -> failwith "Type error"
       )
   | _, _-> failwith "Invalid state"
 
 (* let rec print_state (state:kconf) =
   print_string "Closure:\n";
   match state with 
   | Clos(V x, g), s -> print_string "Variable:"; print_string x; print_string "\n"
   | _ -> () *)
 
 let cl1 = Clos(V "x", [("x", Clos(Tuple([(N 1);(B true)], 2), []))]);;
 krivine (cl1, []) ;;
 
 let cl2 = Clos(App(Abs("x", V("x")), Tuple([], 0)), []) ;;
 krivine (cl2, []) ;;
 
 let cl3 = Clos(Proj(Tuple([Tuple([], 0); (N 1)], 2), 1), []) ;;
 krivine (cl3, []);;
 
 let cl4 = Clos(Abs("y", Add(V "y", (N 4))), []) ;;
 krivine (cl4, [Clos((N 1), [])]);;
 
 