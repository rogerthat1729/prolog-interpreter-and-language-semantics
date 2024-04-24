type termtype = 
    | Program of termtype list
    | Clause of termtype
    | Fact of termtype
    | Rule of termtype * (termtype list)
    | Goal of termtype list
    | Int of int
    | String of string
    | Variable of string
    | Atom of string
    | Bool of bool
    | Vector of string * (termtype list)
    | Nil
    | Fail
