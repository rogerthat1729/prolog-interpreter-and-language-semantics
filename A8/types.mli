type termtype = 
    | Program of termtype list
    | Clause of termtype
    | Fact of termtype
    | Rule of termtype * (termtype list)
    | Goal of (termtype list)
    | Int of int
    | String of string
    | Variable of string
    | Atom of string
    (* | Keyword of string *)
    | Bool of bool
    | Atomicformula of termtype * (termtype list) 
    | List of termtype * termtype
    | Nil
    | Fail
