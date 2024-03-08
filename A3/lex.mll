{
  (* Simply run make in the terminal to run this file *)
  
type arithmeticOp = | Plus| Minus| Times| Divide| Mod| Bitwise_And| Bitwise_Or ;;

type booleanOp = | And| Or| Not| Xor
;;

type comparisonOp = | Less| Greater| Equal| GreaterEqual| LessEqual
;;

type bracket = | Open| Close
;;

type pairOp = | First| Second
;;

type striOp = | Concat| TrimFront| TrimBack| Reverse
;;

type exp = 
  | Paren of bracket 
  | Comma
  | Keyword of string
  | Ident of string
  | Int of int
  | Bool of bool
  | Stri of string (* A string is characterised in "" *)
  | ArOp of arithmeticOp
  | BoolOp of booleanOp
  | CompOp of comparisonOp
  | PairOp of pairOp
  | StriOp of striOp
  | SingleLineComment (* A single line comment is characterised by # *)
  | MultiLineComment (* A multi line comment is characterised by @ ... @ *)
  | Semicolon
  | Colon
  | Dot
  | EOF
  | Error of string
;;

}

let keywords = ("if"|"then"|"else"|"pair"|"let")
let identifier_regex = ('_'|'\''|['a'-'z']|['A'-'Z']|['0'-'9'])*
let identifier_start = ('_'|['a'-'z'])
let identifier_unstart = ('\''|['A'-'Z']|['0'-'9'])

rule token = parse
| '(' {Paren (Open)}
| ')' {Paren (Close)}
| ',' {Comma}
| '.' {Dot}
| ';' {Semicolon}
| ':' {Colon}
| '#'[^'\n']* {SingleLineComment}
| '@'[^'@']*'@' {MultiLineComment}

| '+' {ArOp (Plus)}
| '*' {ArOp (Times)}
| '-' {ArOp (Minus)}
| '/' {ArOp (Divide)}
| "mod" {ArOp (Mod)}
| '&' {ArOp (Bitwise_And)}
| '|' {ArOp (Bitwise_Or)}

(* Currently handling int as 0123 -> 123:INTEGER, no error*)
| ['0'-'9']* as num {Int (int_of_string num)}

| "and" {BoolOp (And)}
| "or" {BoolOp (Or)}
| "not" {BoolOp (Not)}
| "xor" {BoolOp (Xor)}
| "true" {Bool true}
| "false" {Bool false}

| "first" {PairOp First}
| "second" {PairOp Second}

| '\"'[^'\"']*'\"' as s {Stri (String.sub s 1 ((String.length s) - 2))}
| "concat" {StriOp Concat}
| "trimFront" {StriOp TrimFront}
| "trimBack" {StriOp TrimBack}
| "reverse" {StriOp Reverse}

| keywords as s {Keyword s}
| identifier_start identifier_regex as s {Ident s}
| identifier_unstart identifier_regex {Error "Invalid Identifier Name"}

| ">=" {CompOp (GreaterEqual)}
| "<=" {CompOp (LessEqual)}
| '>' {CompOp (Greater)}
| '<' {CompOp (Less)}
| '=' {CompOp (Equal)}

| [' ' '\t' '\n' '\r']+ {token lexbuf}
| eof {EOF}
| _ {Error "Invalid Character"}

{
  
let send_tokens typ str =
  match typ with
  | Paren Open -> Printf.printf "%s: Left Parenthesis\n" str
  | Paren Close -> Printf.printf "%s: Right Parenthesis\n" str
  | Comma -> Printf.printf "%s: Comma\n" str
  | Int num -> Printf.printf "%d: Integer Constant\n" num
  | Bool true -> Printf.printf "true: Boolean Constant (True)\n"
  | Bool false -> Printf.printf "false: Boolean Constant (False)\n"
  | ArOp Plus -> Printf.printf "+: Arithmetic Operator (Plus)\n"
  | ArOp Minus -> Printf.printf "-: Arithmetic Operator (Minus)\n"
  | ArOp Times -> Printf.printf "*: Arithmetic Operator (Times)\n"
  | ArOp Divide -> Printf.printf "/: Arithmetic Operator (Divide)\n"
  | BoolOp And -> Printf.printf "and: Boolean Operator (And)\n"
  | BoolOp Or -> Printf.printf "or: Boolean Operator (Or)\n"
  | BoolOp Not -> Printf.printf "not: Boolean Operator (Not)\n"
  | CompOp Equal -> Printf.printf "=: Comparison Operator (Equal)\n"
  | CompOp Greater -> Printf.printf ">: Comparison Operator (Greater)\n"
  | CompOp Less -> Printf.printf "<: Comparison Operator (Less)\n"
  | Keyword s -> Printf.printf "%s: Keyword '%s'\n" s s
  | Ident s -> Printf.printf "%s: Identifier\n" s
  | Stri s -> Printf.printf "%s: String\n" s
  | StriOp Concat -> Printf.printf "concat: String Concat\n"
  | StriOp TrimFront -> Printf.printf "trimFront: String Trim from front\n"
  | StriOp TrimBack -> Printf.printf "trimBack: String Trim from back\n"
  | StriOp Reverse -> Printf.printf "reverse: String Reverse\n"
  | PairOp First -> Printf.printf "first: First element of pair\n"
  | PairOp Second -> Printf.printf "second: Second element of pair\n"
  | Error s -> Printf.printf "%s: Error (%s)\n" str s
  | EOF -> ()
  
  | Dot -> Printf.printf ".: Dot\n"
  | Colon -> Printf.printf "':': Colon\n"
  | Semicolon -> Printf.printf ";: Semicolon\n"
  | SingleLineComment -> Printf.printf "%s: SingleLineComment\n" str
  | MultiLineComment -> Printf.printf "%s: MultiLineComment\n" str
  | ArOp Mod -> Printf.printf "mod: Arithmetic Operator (Mod)\n"
  | ArOp Bitwise_And -> Printf.printf "&: Arithmetic Operator (Bitwise And)\n"
  | ArOp Bitwise_Or -> Printf.printf "|: Arithmetic Operator (Bitwise Or)\n"
  | BoolOp Xor -> Printf.printf "xor: Boolean Operator (Xor)\n"
  | CompOp GreaterEqual -> Printf.printf ">=: Comparison Operator (GreaterEqual)\n"
  | CompOp LessEqual -> Printf.printf "<=: Comparison Operator (LessEqual)\n"
;;

let () =
  let in_channel = open_in "input.txt" in
  let lexbuf = Lexing.from_channel in_channel in
  try
    while true do
      let result = token lexbuf in
      let expression = Lexing.lexeme lexbuf in
      send_tokens result expression;
      match result with
      | EOF -> raise End_of_file
      | _ -> () 
    done
  with
  | End_of_file -> close_in in_channel 
;;

(* 
Testcases:

let _th'E_n45 ="string";;
let 'def= PQr+_ST-57jkl;;
if a>"b" thentrue else false
let pair("one", 00234): pair1;;
pair1.first= "one" #first element
let s = "abc"concat "def";
@ This is how a multi-
line comment works @
let id1=23&34;
let id2=21 mod 6;
let id3  = 56|78Y*YT34+56;
letid4 = true xor false;
let id5 = 45>=ab23cd;
let id6 = then<=if;
let id7=5 + (63*  (46 -5))

*)

}


