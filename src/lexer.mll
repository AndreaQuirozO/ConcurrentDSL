{
open Parser
exception Eof
exception LexingError of string

let newline lexbuf : unit = Lexing.new_line lexbuf

let info lexbuf =
  let first = Lexing.lexeme_start_p lexbuf in
  let last = Lexing.lexeme_end_p lexbuf in
  ((first.Lexing.pos_lnum, first.Lexing.pos_cnum - first.Lexing.pos_bol),
   (last.Lexing.pos_lnum, last.Lexing.pos_cnum - last.Lexing.pos_bol - 1))

let error lexbuf msg =
  let ((l1,c1),(l2,c2)) = info lexbuf in
  let t = Lexing.lexeme lexbuf in
  let s =
    if l2 = l1
    then Printf.sprintf "line %d, characters %d-%d" l1 c1 c2
    else Printf.sprintf "line %d, character %d, to line %d, character %d" l1 c1 l2 c2 in
  raise (LexingError (Printf.sprintf "%s: lexing error %s at %s." s msg t))
}

let num = ['0'-'9']+
let id  = ['a'-'z'] ['a'-'z' '0'-'9' '_']*
let ws  = [' ' '\t' '\012']

rule token = parse
| ws          { token lexbuf }
| "\r\n"      { newline lexbuf; token lexbuf }
| '\n'        { newline lexbuf; token lexbuf }
| '\r'        { newline lexbuf; token lexbuf }
| "/*"        { comment lexbuf; token lexbuf }
| "+"         { PLUS }
| "-"         { MINUS }
| "*"         { TIMES }
| "("         { LPAREN }
| ")"         { RPAREN }
| "{"         { LBRACE }
| "}"         { RBRACE }
| "="         { EQUALS }
| "!="        { NOTEQUALS }
| "<"         { LESS }
| "<="        { LESSEQ }
| ">"         { GREATER }
| ">="        { GREATEREQ }
| ":="        { ASSIGN }
| ";"         { SEMI }
(* keywords must come before [id]: on equal-length matches, ocamllex picks the first rule *)
| "true"        { TRUE }
| "false"       { FALSE }
| "not"         { NOT }
| "and"         { AND }
| "or"          { OR }
| "skip"        { SKIP }
| "if"          { IF }
| "then"        { THEN }
| "else"        { ELSE }
| "while"       { WHILE }
| "do"          { DO }
| "spawn"       { SPAWN }
| "join"        { JOIN }
| "lock"        { LOCK }
| "unlock"      { UNLOCK }
| "create_lock" { CREATELOCK }
| id as v     { VAR(v) }
| num as n    { INT(int_of_string n) }
| eof         { EOF }
| _ as c      { error lexbuf (Printf.sprintf "unrecognized character %C" c) }

(* C-style comments, which do not nest. *)
and comment = parse
| "*/"        { () }
| "\r\n"      { newline lexbuf; comment lexbuf }
| '\n'        { newline lexbuf; comment lexbuf }
| '\r'        { newline lexbuf; comment lexbuf }
| eof         { error lexbuf "unterminated comment" }
| _           { comment lexbuf }