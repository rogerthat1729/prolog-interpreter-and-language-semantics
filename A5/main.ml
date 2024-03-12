type symbol = string*int

type signature = symbol list

type tree = V of string | C of { node: symbol ; children: tree list }

type substitution = (string * tree) list

let rec check_sig s = 
  match s with
  | [] -> true
  | (a, b)::t -> (b>=0) && not(List.mem (a, b) t) && (check_sig t);;
;;

let rec wftree t = 
  match t with
  | V _ -> true
  | C {node = (a, b); children = l} -> (b = List.length l) && (List.fold_left (&&) true (List.map wftree l))
;;
  
let rec ht t =
  match t with
  | V _ -> 0
  | C {node = (_, 0); children = []} -> 0
  | C {node = (a, b); children = l} -> 1 + List.fold_left max 0 (List.map ht l)
;;

let rec size t =
  match t with
  | V _ -> 1
  | C {node = (_, 0); children = []} -> 1
  | C {node = (a, b); children = l} -> 1 + List.fold_left (+) 0 (List.map size l)
;;

let rec union l1 l2 =
  match l1 with
  | [] -> l2
  | h::t -> if List.mem h l2 then union t l2 else h::(union t l2)
;;

let rec vars t =
  match t with
  | V x -> [x]
  | C {node = (_, 0); children = []} -> []
  | C {node = (a, b); children = l} -> List.fold_left union [] (List.map vars l)
;;

let rec mirror t =
  match t with
  | V _ -> t
  | C {node = (_, 0); children = []} -> t
  | C {node = (a, b); children = l} -> C {node = (a, b); children = List.rev (List.map mirror l)}
;;

(* To check if a table is a valid representation of a function *)
  (* -> There can't be two tuples in the table with same first element - even if the second element is the same, as this is redundant *)

let rec check_table g = 
  match g with
  | [] -> true
  | (a, b)::t -> not(List.mem_assoc a t) && (check_table t)
;;

let rec subst s t =
  match t with
  | V x -> s (V x)
  | C {node = (a, b); children = l} -> C {node = (a, b); children = List.map (subst s) l}
;;

let subst_helper sigma x =
  match x with
  | V x -> (try List.assoc x sigma with Not_found -> V x)
  | _ -> x
;;

let compose_subst sigma1 sigma2 = 
  let all_vars = union (List.map (fun (a, b) -> a) sigma1) (List.map (fun (a, b) -> a) sigma2) in
  List.map (fun x -> (x, subst (subst_helper sigma2) (subst (subst_helper sigma1) (V x)))) all_vars
;;

let mgu t1 t2 =
  let rec mgu_function t1 t2 t =
    match t1, t2 with
    | V x, V y -> subst (subst_helper [(x, V y)]) t
    | V x, C r | C r, V x -> if (List.mem x (vars (C r))) then failwith "NOT_UNIFABLE - variable repeat" else subst (subst_helper [(x, C r)]) t
    | C r1, C r2 -> if r1.node <> r2.node then failwith "NOT_UNIFABLE - different nodes" 
    else 
    (
      let rec recursive_mgu_fn idx l1 l2 t =
        match idx with 
        | 0 -> subst (subst_helper []) t
        | _ -> 
          (
            let sigma = recursive_mgu_fn (idx-1) l1 l2 in
            subst (mgu_function (subst sigma (List.nth l1 (idx-1))) (subst sigma (List.nth l2 (idx-1)))) (subst sigma t)
          )
        in
        recursive_mgu_fn (List.length r1.children) r1.children r2.children t
    )
      in
  let variables = union (vars t1) (vars t2) in
  let init_list = List.map (fun x -> (x, mgu_function t1 t2 (V x))) variables in
  List.filter (fun (a, b) -> b <> (V a)) init_list
;;
  


(* ----------------------------------------------------------------------------------------------------------------------------------  *)




let list_equal l1 l2 = 
  let rec subset l1 l2 =
    match l1 with
    | [] -> true
    | h::t -> (List.mem h l2) && (subset t l2)
  in
  (subset l1 l2) && (subset l2 l1)
;;

let rec print_chars ch indent =
  match indent with 
  | 0 -> ()
  | _ -> print_string ch; print_chars ch (indent-1)
;;

let rec print_tree indent t =
  match t with
  | V x -> print_chars " " indent; print_string "|"; print_string "-Var "; print_string x; print_newline()
  | C {node = (a, b); children = l} -> 
    print_chars " " indent; print_string "|"; print_string "-"; print_string a; print_newline();
    List.iter (fun x -> print_tree (indent+2) x) l
  ;;

let print_vars l = 
  match l with
  | [] -> print_string "[]"
  | _ -> print_string "vars: ["; List.iter (fun x -> print_string x; print_string ";") l; print_string "]\n"
  
let print_list l = 
  match l with
  | [] -> print_string "[]"
  | _ -> print_string "[\n"; List.iter (fun (a, b) ->print_string " "; print_string a; print_string ":\n"; print_tree 0 b) l; print_string "\n]\n"
;;

(* let t3 = V "x";;
let t4 = V "y";;  
let t1 = C {node = ("b", 0); children = []};;
let t2 = C {node = ("c", 1); children = [t1]};;("a", t2); ("b", t1);
let t5 = C {node = ("a", 2); children = [V "p"; V "q"]};;
let t = C {node = ("a", 2) ; children = [t1;t2]};; *)

(* let a = C {node = ("a", 0) ; children = []} ;;
let b = C {node = ("b", 0) ; children = []} ;;

let s2 = C {node = ("b", 0); children = []};;
let s1 = C {node = ("a", 2); children = [s2; V "x"]};;
let p1 = C {node = ("a", 2); children = [V "x"; V "z"]};; *)
  
(* print_endline (string_of_int (ht t3)) *)
(* print_endline (string_of_bool (wftree t)) *)
(* print_tree 0 t;; *)
(* print_tree 0 (mirror t);; *)
(* print_tree 0 (subst [("x", t4)] t);; *)
(* print_tree 0 (subst [(t4, t3)] (subst [(t3, t4)] t));; *)
(* print_tree 0 (compose_subst [(t3, t4)] [(t4, t1)] t);; *)
(* print_list([("x", t3); ("y", t4)]);; *)
(* print_list([("a", t1); ("b", t2)]);; *)
(* print_list(compose [("x", (V "z"))] [("z", a)]);; *)
(* print_list (mgu t2 t3);; *)
(* print_list (mgu hXb hZX);; *)
(* print_list (mgu (V "x") (V "z")) *)
(* print_list (mgu (mirror t) (mirror t5));; *)
(* print_string (string_of_bool (list_equal (mgu t5 t) (mgu (mirror t) (mirror t5))));; *)

(* Write test cases for mgu and other functions *)

(* Testcases *)

(* check_sig *)
 let s = [("a", 0); ("b", 2); ("c", 1); ("a",1)];;
(* print_endline (string_of_bool (check_sig s));;  *)

(* wftree *)
 let t1 = C {node = ("a", 2); children = [V "x"; C {node = ("m", 0); children = [C {node = ("n", 0); children = []}]}]};;
(* print_endline (string_of_bool (wftree t1));;  *)

(* ht, size *)
let t2 = C {node = ("a", 0); children = []};;
let t3 = C {node = ("b", 2); children = [t2; t2]};;
let t4 = C {node = ("c", 2); children = [t2; t3]};;
(* print_endline (string_of_int (ht t4));; *)
(* print_endline (string_of_int (size t4));;  *)

(* vars *)
let t5 = C {node = ("b", 2); children = [V "y"; t2]};;
let t6 = C {node = ("c", 2); children = [V "x"; t5]};;
let t7 = C {node = ("a", 2); children = [t6; V "y"]};;
(* print_vars(vars t7);;  *)

(* mirror *)
(* print_tree 0 (t7);; *)
(* print_tree 0 (mirror t7);; *)

(* check_table *)
(* print_string(string_of_bool (check_table [("a", t1); ("b", t2); ("c", t3)])); print_string "\n";; *)
(* print_string(string_of_bool (check_table [("x", t4); ("y", t5); ("x", t6)])); print_string "\n";; *)

(* composition of substitutions *)
let sigma1 = [("x", t3); ("y", t4); ("z", V "w")];;
let sigma2 = [("w", t2); ("x", t1)];;
(* print_list sigma1;; *)
(* print_list sigma2;; *)
(* print_list(compose_subst sigma1 sigma2);; *)

(* mgu *)

let t8 = C {node = ("b", 0); children = []};;
let t9 = C {node = ("d", 3); children = [V "x"; V "y"; t2]};;
let t10 = C {node = ("d", 3); children = [t8; V "z"; V "z"]};;


let haX = C {node = ("h", 2) ; children = [t2 ; V "x"]} ;;
let hXb = C {node = ("h", 2) ; children = [ V "x" ; t8]} ;;
let t1 = C {node = ("f", 2) ; children = [haX ; hXb]} ;;
let hab = C {node = ("h", 2) ; children = [t2 ; t8]} ;;
let hZX = C {node = ("h", 2) ; children = [V "z" ; V "x" ]} ;;
let t11 = C {node = ("f", 2) ; children = [hab ; hZX]} ;;
let t12 = C {node = ("f", 2); children = [haX; hXb]} ;;

let t13 = C {node = ("a", 2); children = [V "x"; V "y"]};;
let t14 = C {node = ("b", 2); children = [V "x"; V "x"]};;

let t15 = V "j";;
let t16 = C {node = ("a", 2); children = [V "x"; V "j"]};;

(* print_list (mgu t9 t10);; *)
(* print_list (mgu t11 t12) ;; *)
(* print_list (mgu t13 t14) ;; *)
(* print_list (mgu t15 t16);; *)