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
;;

let rec compile e =
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
| _ -> []
;;

let rec execute s g (c:opcode list) d =
  match s, g, c, d with
  | _ -> []

