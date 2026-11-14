open Dsl
open Helpers

let tokens s =
  let lb = Lexing.from_string s in
  let rec go acc =
    match Lexer.token lb with
    | Parser.EOF -> List.rev (Parser.EOF :: acc)
    | t -> go (t :: acc) in
  go []

let expect_tokens name src toks = check name (try tokens src = toks with _ -> false)
let lex_fails name src =
  check name (try ignore (tokens src); false with Lexer.LexingError _ -> true)

let () =
  let open Parser in
  (* keywords *)
  expect_tokens "keywords"
    "skip if then else while do spawn join lock unlock create_lock"
    [SKIP; IF; THEN; ELSE; WHILE; DO; SPAWN; JOIN; LOCK; UNLOCK; CREATELOCK; EOF];
  expect_tokens "boolean keywords" "true false not and or"
    [TRUE; FALSE; NOT; AND; OR; EOF];

  (* operators and punctuation *)
  expect_tokens "operators" "+ - * ( ) { } = != < <= > >= := ;"
    [PLUS; MINUS; TIMES; LPAREN; RPAREN; LBRACE; RBRACE; EQUALS; NOTEQUALS;
     LESS; LESSEQ; GREATER; GREATEREQ; ASSIGN; SEMI; EOF];
  expect_tokens "'<=' is one token, '< =' is two" "<= < ="
    [LESSEQ; LESS; EQUALS; EOF];
  expect_tokens "':=' and ';' without spaces" "x:=1;"
    [VAR "x"; ASSIGN; INT 1; SEMI; EOF];
  expect_tokens "'!=' vs '='" "!= ="
    [NOTEQUALS; EQUALS; EOF];

  (* identifiers *)
  expect_tokens "identifiers containing keywords are VARs"
    "locked unlocked lock_a joined spawned createlock iff doo"
    [VAR "locked"; VAR "unlocked"; VAR "lock_a"; VAR "joined"; VAR "spawned";
     VAR "createlock"; VAR "iff"; VAR "doo"; EOF];
  expect_tokens "digits and underscores in identifiers" "x1 a_b2"
    [VAR "x1"; VAR "a_b2"; EOF];
  expect_tokens "create_lock is one token, not create / _ / lock" "create_lock"
    [CREATELOCK; EOF];

  (* integers *)
  expect_tokens "integers" "0 42 007" [INT 0; INT 42; INT 7; EOF];
  expect_tokens "minus is a separate token" "-5" [MINUS; INT 5; EOF];

  (* whitespace and comments *)
  expect_tokens "whitespace and newlines ignored" " x \n\t y\r\n z "
    [VAR "x"; VAR "y"; VAR "z"; EOF];
  expect_tokens "comment skipped" "x /* hi\n there */ y" [VAR "x"; VAR "y"; EOF];
  expect_tokens "comment with stars inside" "x /* ** * */ y" [VAR "x"; VAR "y"; EOF];
  expect_tokens "empty input" "" [EOF];
  expect_tokens "only a comment" "/* nothing */" [EOF];

  (* errors *)
  lex_fails "unrecognized character" "x # y";
  lex_fails "uppercase identifier rejected" "X";
  lex_fails "unterminated comment" "x /* oops";
  lex_fails "lone '!' rejected" "x ! y";
  check "error message reports the right line"
    (try ignore (tokens "x\n\n#"); false
     with Lexer.LexingError m -> contains m "line 3");
  check "error message reports line after a comment"
    (try ignore (tokens "/* a\nb */\n#"); false
     with Lexer.LexingError m -> contains m "line 3")

let () = finish "lexer"