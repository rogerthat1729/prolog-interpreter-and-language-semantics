type symbol = string*int

type signature = symbol list

type tree = V of string | C of { node: symbol ; children: tree list }

type substitution = (tree * tree) list

let rec check_sig s = 
  match s with
  | [] -> true
  | (a, b)::t -> (b>=0) && (check_sig t) && not(List.mem (a, b) t);;

let rec wftree t = 
  match t with
  | V _ -> true
  | C {node = (a, b); children = l} ->
    if b == List.length l then List.fold_left (&&) true (List.map wftree l) else false
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

let rec subst sigma t =
  match t with
  | V x -> (try List.assoc t sigma with Not_found -> t)
  | C {node = (a, b); children = l} -> C {node = (a, b); children = List.map (subst sigma) l}
;;

let compose_subst sigma1 sigma2 t = subst sigma2 (subst sigma1 t)
;;

let t3 = V "x";;
let t4 = V "y";;  
let t1 = C {node = ("b", 0); children = []};;
let t2 = C {node = ("c", 1); children = [t3]};;
let t = C {node = ("a", 2) ; children = [t1;t2]};;

let rec print_chars ch indent =
  match indent with 
  | 0 -> ()
  | _ -> print_string ch; print_chars ch (indent-1)
;;
let rec print_tree indent t =
  match t with
  | V x -> print_chars " " (indent+2); print_string "|"; print_string "-Var "; print_string x; print_newline()
  | C {node = (a, b); children = l} -> 
    print_chars " " (indent+2); print_string "|"; print_string "-"; print_string a; print_newline();
    List.iter (fun x -> print_tree (indent+2) x) l
  ;;
  
  
(* print_endline (string_of_int (ht t3)) *)
(* print_endline (string_of_bool (wftree t)) *)
(* print_tree 0 t;;
print_tree 0 (mirror t);; *)
(* print_tree 0 (subst [(t3, t4)] t);; *)
(* print_tree 0 (subst [(t4, t3)] (subst [(t3, t4)] t));; *)
(* print_tree 0 (compose_subst [(t3, t4)] [(t4, t1)] t);; *)


