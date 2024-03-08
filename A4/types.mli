type termtype = 
    | Program of termtype
    | Clause of termtype
    | Fact of termtype
    | Rule of termtype * termtype
    | Head of termtype
    | Body of termtype
    | Goal of termtype
    | Int of int
    | String of string
    | Variable of string
    | Atom of string
    | Keyword of string
    | Bool of bool
    | Termlist of termtype list
    | Atomicformula of (string * termtype) 
    | Funct of (string * termtype)
    | Op of string * string * termtype
