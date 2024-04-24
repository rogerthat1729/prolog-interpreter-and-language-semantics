
{
    open Parser
    open String
    exception Error of char
}

let operator = ("="|"<"|">"|"=/=")

rule token = parse
     [' ' '\t']
       { token lexbuf }
   | ['\n']
       { Lexing.new_line lexbuf; token lexbuf }
    | [',']
        { COMMA }
    | ['.']
        { DOT }
    | ":-"
        { COLON_DASH }
    | "?-"
        { Q_DASH }
    | '|'
        { PIPE }
    | [';']
        { SEMICOLON }
    | ['_']
        { UNDERSCORE }
    | "fail"
        { FAIL }
    | operator as lxm
        { OP lxm }
   | ['0'-'9']+ as lxm
       { INT (int_of_string lxm) }
    | ("true"|"false") as lxm
        { BOOL (bool_of_string lxm) }
    | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* as lxm
        { IDENT lxm }
    | ['a'-'z']+ as lxm
        { ATOM lxm }
    | '\"'[^'\"']*'\"' as lxm
        { STRING (String.sub lxm 1 ((String.length lxm)-2)) }
   | ['(']
       { LPAREN }
   | [')']
       { RPAREN }
    | ['[']
        { LSQUARE }
    | [']']
        { RSQUARE }
    | '#'[^'\n']*'\n'
        { token lexbuf }
   | eof
       { EOF }
   | _ as s
       { raise (Error s) }