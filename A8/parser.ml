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
\005\000\007\000\007\000\008\000\008\000\008\000\008\000\009\000\
\009\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\011\000\011\000\012\000\012\000\012\000\000\000"

let yylen = "\002\000\
\002\000\002\000\003\000\001\000\001\000\002\000\001\000\001\000\
\003\000\001\000\003\000\001\000\004\000\001\000\003\000\001\000\
\003\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\002\000\003\000\001\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\018\000\019\000\021\000\000\000\022\000\000\000\
\020\000\014\000\000\000\030\000\000\000\000\000\004\000\005\000\
\000\000\000\000\023\000\000\000\006\000\007\000\000\000\025\000\
\024\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\026\000\003\000\009\000\015\000\
\013\000\000\000\011\000\028\000\000\000\017\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000\015\000\016\000\021\000\022\000\025\000\
\032\000\018\000\019\000\027\000"

let yysindex = "\013\000\
\046\255\000\000\000\000\000\000\000\000\255\254\000\000\002\255\
\000\000\000\000\033\255\000\000\016\000\006\255\000\000\000\000\
\011\255\024\255\000\000\002\255\000\000\000\000\026\255\000\000\
\000\000\007\255\020\255\000\000\046\255\002\255\002\255\027\255\
\252\254\002\255\002\255\002\255\000\000\000\000\000\000\000\000\
\000\000\002\255\000\000\000\000\024\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\054\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\012\255\000\000\000\000\000\000\000\000\000\000\035\255\000\000\
\000\000\038\255\000\000\000\000\045\000\000\000\000\000\000\000\
\041\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\040\255\000\000"

let yygindex = "\000\000\
\000\000\032\000\000\000\000\000\000\000\000\000\253\255\255\255\
\020\000\246\255\000\000\028\000"

let yytablesize = 73
let yytable = "\017\000\
\026\000\031\000\003\000\004\000\005\000\006\000\023\000\042\000\
\007\000\033\000\009\000\010\000\031\000\001\000\020\000\028\000\
\029\000\024\000\035\000\011\000\040\000\036\000\008\000\030\000\
\026\000\045\000\039\000\017\000\023\000\031\000\043\000\033\000\
\023\000\003\000\004\000\005\000\006\000\034\000\037\000\007\000\
\024\000\009\000\010\000\041\000\002\000\010\000\003\000\004\000\
\005\000\006\000\011\000\024\000\007\000\008\000\009\000\010\000\
\027\000\016\000\029\000\012\000\038\000\046\000\044\000\011\000\
\012\000\012\000\012\000\000\000\012\000\000\000\012\000\000\000\
\012\000"

let yycheck = "\001\000\
\011\000\006\001\001\001\002\001\003\001\004\001\008\000\012\001\
\007\001\020\000\009\001\010\001\006\001\001\000\016\001\000\000\
\011\001\006\001\012\001\018\001\031\000\015\001\011\001\013\001\
\035\000\036\000\030\000\029\000\030\000\006\001\034\000\042\000\
\034\000\001\001\002\001\003\001\004\001\012\001\019\001\007\001\
\006\001\009\001\010\001\017\001\000\000\011\001\001\001\002\001\
\003\001\004\001\018\001\019\001\007\001\008\001\009\001\010\001\
\019\001\017\001\019\001\006\001\029\000\042\000\035\000\018\001\
\011\001\012\001\013\001\255\255\015\001\255\255\017\001\255\255\
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
# 157 "parser.ml"
               : Types.termtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause) in
    Obj.repr(
# 29 "parser.mly"
                (Program [_1])
# 164 "parser.ml"
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
# 176 "parser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 38 "parser.mly"
         (Clause _1)
# 183 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rule) in
    Obj.repr(
# 39 "parser.mly"
         (Clause _1)
# 190 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'goal) in
    Obj.repr(
# 40 "parser.mly"
                (_2)
# 197 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aflist) in
    Obj.repr(
# 43 "parser.mly"
           (Goal _1)
# 204 "parser.ml"
               : 'goal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'af) in
    Obj.repr(
# 46 "parser.mly"
       (Fact _1)
# 211 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'af) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aflist) in
    Obj.repr(
# 49 "parser.mly"
                         (Rule (_1, _3))
# 219 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'af) in
    Obj.repr(
# 52 "parser.mly"
       ([_1])
# 226 "parser.ml"
               : 'aflist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'af) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aflist) in
    Obj.repr(
# 53 "parser.mly"
                    ( _1 :: _3 )
# 234 "parser.ml"
               : 'aflist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
         (Vector(_1, []))
# 241 "parser.ml"
               : 'af))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'tlist) in
    Obj.repr(
# 57 "parser.mly"
                             (Vector(_1, _3))
# 249 "parser.ml"
               : 'af))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
         (Fail)
# 255 "parser.ml"
               : 'af))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 59 "parser.mly"
                 (Vector(_2, [_1;_3]))
# 264 "parser.ml"
               : 'af))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 62 "parser.mly"
         ([_1])
# 271 "parser.ml"
               : 'tlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tlist) in
    Obj.repr(
# 64 "parser.mly"
  ( _1 :: _3 )
# 279 "parser.ml"
               : 'tlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 67 "parser.mly"
        (Int _1)
# 286 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 68 "parser.mly"
         (Bool _1)
# 293 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
               (Variable "@")
# 299 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "parser.mly"
          (Variable _1)
# 306 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "parser.mly"
           (String _1)
# 313 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lst) in
    Obj.repr(
# 72 "parser.mly"
        ( _1 )
# 320 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'af) in
    Obj.repr(
# 73 "parser.mly"
       ( _1 )
# 327 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                    (Nil)
# 333 "parser.ml"
               : 'lst))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lst1) in
    Obj.repr(
# 77 "parser.mly"
                         (_2)
# 340 "parser.ml"
               : 'lst))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 80 "parser.mly"
         (Vector ("_LST", [_1; Nil]))
# 347 "parser.ml"
               : 'lst1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lst1) in
    Obj.repr(
# 81 "parser.mly"
                    (Vector ("_LST", [_1; _3]))
# 355 "parser.ml"
               : 'lst1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 82 "parser.mly"
                   (Vector ("_LST", [_1; _3]))
# 363 "parser.ml"
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
