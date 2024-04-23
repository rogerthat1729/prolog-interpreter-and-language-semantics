type token =
  | INT of (int)
  | BOOL of (bool)
  | IDENT of (string)
  | ATOM of (string)
  | KEYWORD of (string)
  | OP of (string)
  | STRING of (string)
  | Q_DASH
  | UNDERSCORE
  | FAIL
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
# 28 "parser.ml"
let yytransl_const = [|
  264 (* Q_DASH *);
  265 (* UNDERSCORE *);
  266 (* FAIL *);
  267 (* DOT *);
  268 (* COMMA *);
  269 (* COLON_DASH *);
  270 (* SEMICOLON *);
  271 (* PIPE *);
  272 (* LPAREN *);
  273 (* RPAREN *);
  274 (* LSQUARE *);
  275 (* RSQUARE *);
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
\005\000\007\000\007\000\008\000\008\000\008\000\009\000\009\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\011\000\
\011\000\012\000\012\000\012\000\000\000"

let yylen = "\002\000\
\002\000\002\000\003\000\001\000\001\000\002\000\001\000\001\000\
\003\000\001\000\003\000\001\000\004\000\001\000\001\000\003\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\003\000\001\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\014\000\029\000\000\000\000\000\
\004\000\005\000\000\000\000\000\006\000\007\000\000\000\001\000\
\000\000\000\000\017\000\018\000\020\000\021\000\019\000\000\000\
\023\000\000\000\000\000\022\000\000\000\003\000\009\000\024\000\
\000\000\000\000\013\000\000\000\011\000\000\000\000\000\025\000\
\016\000\027\000\028\000"

let yydgoto = "\002\000\
\006\000\007\000\008\000\009\000\010\000\013\000\014\000\025\000\
\026\000\027\000\028\000\034\000"

let yysindex = "\014\000\
\034\255\000\000\241\254\254\254\000\000\000\000\018\000\016\255\
\000\000\000\000\017\255\022\255\000\000\000\000\033\255\000\000\
\034\255\254\254\000\000\000\000\000\000\000\000\000\000\003\255\
\000\000\029\255\035\255\000\000\254\254\000\000\000\000\000\000\
\255\254\030\255\000\000\022\255\000\000\022\255\022\255\000\000\
\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\024\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\037\255\000\000\000\000\000\000\039\255\000\000\
\051\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\255\000\000\000\000\000\000\000\000\000\000\
\038\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\035\000\000\000\000\000\000\000\000\000\247\255\255\255\
\019\000\251\255\000\000\016\000"

let yytablesize = 57
let yytable = "\011\000\
\012\000\003\000\015\000\019\000\020\000\021\000\003\000\005\000\
\031\000\022\000\038\000\023\000\005\000\039\000\001\000\011\000\
\015\000\016\000\033\000\037\000\024\000\032\000\019\000\020\000\
\021\000\003\000\017\000\015\000\022\000\018\000\023\000\005\000\
\033\000\043\000\012\000\012\000\012\000\003\000\012\000\024\000\
\012\000\004\000\012\000\005\000\029\000\035\000\036\000\008\000\
\040\000\010\000\002\000\030\000\015\000\042\000\041\000\000\000\
\026\000"

let yycheck = "\001\000\
\016\001\004\001\004\000\001\001\002\001\003\001\004\001\010\001\
\018\000\007\001\012\001\009\001\010\001\015\001\001\000\017\000\
\018\000\000\000\024\000\029\000\018\001\019\001\001\001\002\001\
\003\001\004\001\011\001\029\000\007\001\013\001\009\001\010\001\
\038\000\039\000\011\001\012\001\013\001\004\001\015\001\018\001\
\017\001\008\001\019\001\010\001\012\001\017\001\012\001\011\001\
\019\001\011\001\000\000\017\000\017\001\038\000\036\000\255\255\
\019\001"

let yynames_const = "\
  Q_DASH\000\
  UNDERSCORE\000\
  FAIL\000\
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
# 153 "parser.ml"
               : Types.termtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause) in
    Obj.repr(
# 29 "parser.mly"
                (Program [_1])
# 160 "parser.ml"
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
# 172 "parser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 38 "parser.mly"
         (Clause _1)
# 179 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rule) in
    Obj.repr(
# 39 "parser.mly"
         (Clause _1)
# 186 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'goal) in
    Obj.repr(
# 40 "parser.mly"
                (_2)
# 193 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aflist) in
    Obj.repr(
# 43 "parser.mly"
           (Goal _1)
# 200 "parser.ml"
               : 'goal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'af) in
    Obj.repr(
# 46 "parser.mly"
       (Fact _1)
# 207 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'af) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aflist) in
    Obj.repr(
# 49 "parser.mly"
                         (Rule (_1, _3))
# 215 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'af) in
    Obj.repr(
# 52 "parser.mly"
       ([_1])
# 222 "parser.ml"
               : 'aflist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'af) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aflist) in
    Obj.repr(
# 53 "parser.mly"
                    ( _1 :: _3 )
# 230 "parser.ml"
               : 'aflist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
         (Atomicformula (Atom(_1), []))
# 237 "parser.ml"
               : 'af))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'tlist) in
    Obj.repr(
# 57 "parser.mly"
                             (Atomicformula (Atom(_1), _3))
# 245 "parser.ml"
               : 'af))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
         (Fail)
# 251 "parser.ml"
               : 'af))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 61 "parser.mly"
         ([_1])
# 258 "parser.ml"
               : 'tlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tlist) in
    Obj.repr(
# 62 "parser.mly"
                     ( _1 :: _3 )
# 266 "parser.ml"
               : 'tlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 65 "parser.mly"
        (Int _1)
# 273 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 66 "parser.mly"
         (Bool _1)
# 280 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
               (Variable "@")
# 286 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "parser.mly"
          (Variable _1)
# 293 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "parser.mly"
           (String _1)
# 300 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lst) in
    Obj.repr(
# 71 "parser.mly"
        ( _1 )
# 307 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'af) in
    Obj.repr(
# 72 "parser.mly"
       ( _1 )
# 314 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                    (Nil)
# 320 "parser.ml"
               : 'lst))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lst1) in
    Obj.repr(
# 76 "parser.mly"
                         (_2)
# 327 "parser.ml"
               : 'lst))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 79 "parser.mly"
         (List (_1, Nil))
# 334 "parser.ml"
               : 'lst1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lst1) in
    Obj.repr(
# 80 "parser.mly"
                    (List (_1, _3))
# 342 "parser.ml"
               : 'lst1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 81 "parser.mly"
                   (List (_1, _3))
# 350 "parser.ml"
               : 'lst1))
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
