type token =
  | INT of (int)
  | BOOL of (bool)
  | IDENT of (string)
  | ATOM of (string)
  | KEYWORD of (string)
  | OP of (string)
  | STRING of (string)
  | Q_DASH
  | DOT
  | COMMA
  | COLON_DASH
  | SEMICOLON
  | PIPE
  | LPAREN
  | RPAREN
  | LSQUARE
  | RSQUARE
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Types
# 26 "parser.ml"
let yytransl_const = [|
  264 (* Q_DASH *);
  265 (* DOT *);
  266 (* COMMA *);
  267 (* COLON_DASH *);
  268 (* SEMICOLON *);
  269 (* PIPE *);
  270 (* LPAREN *);
  271 (* RPAREN *);
  272 (* LSQUARE *);
  273 (* RSQUARE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* IDENT *);
  260 (* ATOM *);
  261 (* KEYWORD *);
  262 (* OP *);
  263 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\006\000\004\000\
\005\000\007\000\007\000\008\000\008\000\009\000\009\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\011\000\011\000\
\011\000\012\000\012\000\000\000"

let yylen = "\002\000\
\002\000\002\000\003\000\001\000\001\000\002\000\001\000\001\000\
\003\000\001\000\003\000\001\000\004\000\001\000\003\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000\005\000\
\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\028\000\000\000\000\000\004\000\
\005\000\000\000\000\000\006\000\007\000\000\000\001\000\000\000\
\000\000\016\000\017\000\019\000\018\000\020\000\000\000\022\000\
\000\000\000\000\021\000\000\000\003\000\009\000\023\000\000\000\
\000\000\013\000\000\000\011\000\000\000\000\000\025\000\015\000\
\000\000\027\000\000\000\024\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\009\000\012\000\013\000\024\000\
\025\000\026\000\027\000\033\000"

let yysindex = "\001\000\
\005\255\000\000\004\255\017\255\000\000\029\000\026\255\000\000\
\000\000\019\255\021\255\000\000\000\000\029\255\000\000\005\255\
\017\255\000\000\000\000\000\000\000\000\000\000\003\255\000\000\
\027\255\031\255\000\000\017\255\000\000\000\000\000\000\001\255\
\028\255\000\000\021\255\000\000\021\255\030\255\000\000\000\000\
\033\255\000\000\032\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\023\255\000\000\000\000\000\000\000\000\000\000\
\000\000\035\255\000\000\000\000\000\000\038\255\000\000\048\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\255\000\000\000\000\000\000\000\000\000\000\037\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\037\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\034\000\000\000\000\000\000\000\000\000\240\255\255\255\
\017\000\250\255\015\000\018\000"

let yytablesize = 55
let yytable = "\010\000\
\030\000\001\000\014\000\018\000\019\000\020\000\003\000\021\000\
\003\000\022\000\037\000\036\000\004\000\038\000\010\000\014\000\
\032\000\011\000\023\000\031\000\003\000\018\000\019\000\020\000\
\003\000\021\000\014\000\022\000\015\000\017\000\041\000\012\000\
\012\000\012\000\016\000\012\000\023\000\012\000\028\000\012\000\
\035\000\034\000\037\000\008\000\039\000\023\000\010\000\002\000\
\044\000\029\000\014\000\040\000\043\000\026\000\042\000"

let yycheck = "\001\000\
\017\000\001\000\004\000\001\001\002\001\003\001\004\001\005\001\
\004\001\007\001\010\001\028\000\008\001\013\001\016\000\017\000\
\023\000\014\001\016\001\017\001\004\001\001\001\002\001\003\001\
\004\001\005\001\028\000\007\001\000\000\011\001\037\000\009\001\
\010\001\011\001\009\001\013\001\016\001\015\001\010\001\017\001\
\010\001\015\001\010\001\009\001\017\001\016\001\009\001\000\000\
\017\001\016\000\015\001\035\000\038\000\017\001\037\000"

let yynames_const = "\
  Q_DASH\000\
  DOT\000\
  COMMA\000\
  COLON_DASH\000\
  SEMICOLON\000\
  PIPE\000\
  LPAREN\000\
  RPAREN\000\
  LSQUARE\000\
  RSQUARE\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  IDENT\000\
  ATOM\000\
  KEYWORD\000\
  OP\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'program) in
    Obj.repr(
# 26 "parser.mly"
    ( _1 )
# 145 "parser.ml"
               : Types.termtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause) in
    Obj.repr(
# 29 "parser.mly"
                (Program [_1])
# 152 "parser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'clause) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 31 "parser.mly"
  (
      match _3 with
    | Program l -> Program (_1::l)
    | _ -> failwith "Parsing Error"
  )
# 164 "parser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 38 "parser.mly"
         (Clause _1)
# 171 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rule) in
    Obj.repr(
# 39 "parser.mly"
         (Clause _1)
# 178 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'goal) in
    Obj.repr(
# 40 "parser.mly"
                (_2)
# 185 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aflist) in
    Obj.repr(
# 43 "parser.mly"
           (Goal _1)
# 192 "parser.ml"
               : 'goal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'af) in
    Obj.repr(
# 46 "parser.mly"
       (Fact _1)
# 199 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'af) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aflist) in
    Obj.repr(
# 49 "parser.mly"
                         (Rule (_1, _3))
# 207 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'af) in
    Obj.repr(
# 52 "parser.mly"
       ([_1])
# 214 "parser.ml"
               : 'aflist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'af) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aflist) in
    Obj.repr(
# 53 "parser.mly"
                    ( _1 :: _3 )
# 222 "parser.ml"
               : 'aflist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
         (Atomicformula (Atom(_1), []))
# 229 "parser.ml"
               : 'af))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'tlist) in
    Obj.repr(
# 57 "parser.mly"
                             (Atomicformula (Atom(_1), _3))
# 237 "parser.ml"
               : 'af))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 60 "parser.mly"
         ([_1])
# 244 "parser.ml"
               : 'tlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tlist) in
    Obj.repr(
# 61 "parser.mly"
                     ( _1 :: _3 )
# 252 "parser.ml"
               : 'tlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 64 "parser.mly"
        (Int _1)
# 259 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 65 "parser.mly"
         (Bool _1)
# 266 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
            (Keyword _1)
# 273 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
          (Variable _1)
# 280 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
           (String _1)
# 287 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lst) in
    Obj.repr(
# 69 "parser.mly"
        ( _1 )
# 294 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'af) in
    Obj.repr(
# 70 "parser.mly"
       ( _1 )
# 301 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                    (List [])
# 307 "parser.ml"
               : 'lst))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'lst) in
    Obj.repr(
# 75 "parser.mly"
  (
    match _4 with
    | List l -> List (_2::l)
    | _ -> failwith "Parsing Error"
  )
# 319 "parser.ml"
               : 'lst))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'commalist) in
    Obj.repr(
# 80 "parser.mly"
                              (List _2)
# 326 "parser.ml"
               : 'lst))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 83 "parser.mly"
         ([_1])
# 333 "parser.ml"
               : 'commalist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'commalist) in
    Obj.repr(
# 84 "parser.mly"
                         ( _1 :: _3 )
# 341 "parser.ml"
               : 'commalist))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Types.termtype)
