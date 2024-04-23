type termtype = 
    | Program of termtype list
    | Clause of termtype
    | Fact of termtype
    | Rule of termtype * (termtype list)
    (* | Head of termtype
    | Body of (termtype list) *)
    | Goal of (termtype list)
    | Int of int
    | String of string
    | Variable of string
    | Atom of string
    | Keyword of string
    | Bool of bool
    (* | Termlist of termtype list *)
    | Atomicformula of termtype * (termtype list) 
    | List of (termtype list)
    (* | Funct of (string * termtype) *)
    (* | Op of string * string * termtype *)
