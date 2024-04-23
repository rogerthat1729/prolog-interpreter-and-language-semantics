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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Types.termtype
