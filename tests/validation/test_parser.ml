open Dsl
open Ast
open Helpers

let parse s = Parser.program Lexer.token (Lexing.from_string s)

let expect_parse name src ast = check name (try parse src = ast with _ -> false)
let expect_fail name src =
  check name
    (try ignore (parse src); false with
     | Parsing.Parse_error | Lexer.LexingError _ -> true)

let rec seq = function
  | [] -> Skip | [c] -> c | c :: r -> Seq (c, seq r)

(* ---------- arithmetic ---------- *)
let () =
  expect_parse "assign int" "x := 5" (Assign ("x", Int 5));
  expect_parse "negative literal" "x := -5" (Assign ("x", Int (-5)));
  expect_parse "variable on rhs" "x := y" (Assign ("x", Var "y"));
  expect_parse "* binds tighter than +" "x := 1 + 2 * 3"
    (Assign ("x", Plus (Int 1, Times (Int 2, Int 3))));
  expect_parse "+ is left-assoc" "x := 1 + 2 + 3"
    (Assign ("x", Plus (Plus (Int 1, Int 2), Int 3)));
  expect_parse "- is left-assoc" "x := 1 - 2 - 3"
    (Assign ("x", Minus (Minus (Int 1, Int 2), Int 3)));
  expect_parse "* is left-assoc" "x := 2 * 3 * 4"
    (Assign ("x", Times (Times (Int 2, Int 3), Int 4)));
  expect_parse "parentheses override" "x := (1 + 2) * 3"
    (Assign ("x", Times (Plus (Int 1, Int 2), Int 3)));
  expect_parse "subtract a negative" "x := 3 - -1"
    (Assign ("x", Minus (Int 3, Int (-1))))

(* ---------- booleans ---------- *)
let () =
  let cond b = parse ("if " ^ b ^ " then { skip } else { skip }") in
  let mk b = If (b, Skip, Skip) in
  let is name b expected = check name (try cond b = mk expected with _ -> false) in
  is "true" "true" True;
  is "false" "false" False;
  is "=" "x = 1" (Equals (Var "x", Int 1));
  is "!=" "x != 1" (NotEquals (Var "x", Int 1));
  is "<" "x < 1" (Less (Var "x", Int 1));
  is "<=" "x <= 1" (LessEq (Var "x", Int 1));
  is ">" "x > 1" (Greater (Var "x", Int 1));
  is ">=" "x >= 1" (GreaterEq (Var "x", Int 1));
  is "and binds tighter than or" "true or false and true"
    (Or (True, And (False, True)));
  is "or is left-assoc" "true or false or true"
    (Or (Or (True, False), True));
  is "and is left-assoc" "true and false and true"
    (And (And (True, False), True));
  is "not binds tighter than and" "not true and false"
    (And (Not True, False));
  is "double not" "not not true" (Not (Not True));
  is "parenthesised boolean" "(true or false) and true"
    (And (Or (True, False), True));
  is "arithmetic inside comparison" "x + 1 < y * 2"
    (Less (Plus (Var "x", Int 1), Times (Var "y", Int 2)));
  is "parenthesised arithmetic in comparison" "(x + 1) < 2"
    (Less (Plus (Var "x", Int 1), Int 2));
  is "parenthesised comparison" "(x < 1)" (Less (Var "x", Int 1))

(* ---------- commands ---------- *)
let () =
  expect_parse "skip" "skip" Skip;
  expect_parse "seq is right-nested" "skip; skip; skip"
    (Seq (Skip, Seq (Skip, Skip)));
  expect_parse "trailing semicolon" "skip;" Skip;
  expect_parse "if/else" "if x < 1 then { y := 1 } else { y := 2 }"
    (If (Less (Var "x", Int 1), Assign ("y", Int 1), Assign ("y", Int 2)));
  expect_parse "while" "while x < 3 do { x := x + 1 }"
    (While (Less (Var "x", Int 3), Assign ("x", Plus (Var "x", Int 1))));
  expect_parse "empty block is skip" "while false do { }" (While (False, Skip));
  expect_parse "block with seq" "while false do { skip; skip }"
    (While (False, Seq (Skip, Skip)));
  expect_parse "block with trailing semicolon" "while false do { skip; }"
    (While (False, Skip));
  expect_parse "nested if in while"
    "while true do { if true then { skip } else { skip } }"
    (While (True, If (True, Skip, Skip)));
  expect_parse "statement after a block needs ';'"
    "while false do { skip }; x := 1"
    (Seq (While (False, Skip), Assign ("x", Int 1)));
  expect_parse "comments ignored" "skip /* c */ ; /* d */ skip"
    (Seq (Skip, Skip))

(* ---------- concurrency and locks ---------- *)
let () =
  expect_parse "create_lock" "a := create_lock()" (CreateLock "a");
  expect_parse "lock" "lock a" (Lock "a");
  expect_parse "unlock" "unlock a" (Unlock "a");
  expect_parse "join" "join t" (Join "t");
  expect_parse "spawn" "t := spawn { x := 1 }"
    (Spawn ("t", Assign ("x", Int 1)));
  expect_parse "spawn empty body" "t := spawn { }" (Spawn ("t", Skip));
  expect_parse "spawn body is a sequence" "t := spawn { lock a; unlock a }"
    (Spawn ("t", Seq (Lock "a", Unlock "a")));
  expect_parse "nested spawn" "t := spawn { u := spawn { skip }; join u }"
    (Spawn ("t", Seq (Spawn ("u", Skip), Join "u")));
  expect_parse "spawn inside while" "while true do { t := spawn { skip } }"
    (While (True, Spawn ("t", Skip)));
  expect_parse "lock/unlock inside if"
    "if true then { lock a } else { unlock a }"
    (If (True, Lock "a", Unlock "a"))

(* ---------- full program ---------- *)
let () =
  let src = {|
    /* two threads, two locks, same acquisition order */
    a := create_lock();
    b := create_lock();
    x := 0;
    t := spawn {
      lock a; lock b;
      x := x + 1;
      unlock b; unlock a
    };
    lock a; lock b;
    x := x + 10;
    unlock b; unlock a;
    join t
  |} in
  expect_parse "full two-thread program" src
    (seq [ CreateLock "a"; CreateLock "b"; Assign ("x", Int 0);
           Spawn ("t",
                  seq [ Lock "a"; Lock "b";
                        Assign ("x", Plus (Var "x", Int 1));
                        Unlock "b"; Unlock "a" ]);
           Lock "a"; Lock "b";
           Assign ("x", Plus (Var "x", Int 10));
           Unlock "b"; Unlock "a";
           Join "t" ])

(* ---------- must be rejected ---------- *)
let () =
  expect_fail "empty program" "";
  expect_fail "missing semicolon between statements" "skip skip";
  expect_fail "double semicolon" "skip;; skip";
  expect_fail "leading semicolon" "; skip";
  expect_fail "unbraced if body" "if true then skip else skip";
  expect_fail "if without else" "if true then { skip }";
  expect_fail "unbraced while body" "while true do skip";
  expect_fail "while without do" "while true { skip }";
  expect_fail "spawn without assignment" "spawn { skip }";
  expect_fail "spawn without braces" "t := spawn skip";
  expect_fail "create_lock without parens" "a := create_lock";
  expect_fail "create_lock with argument" "a := create_lock(1)";
  expect_fail "lock on a number" "lock 3";
  expect_fail "join without variable" "join";
  expect_fail "'=' is not assignment" "x = 1";
  expect_fail "assignment missing rhs" "x :=";
  expect_fail "keyword as variable" "lock := 1";
  expect_fail "unbalanced paren" "x := (1 + 2";
  expect_fail "unbalanced brace" "t := spawn { skip";
  expect_fail "boolean where arithmetic expected" "x := true";
  expect_fail "arithmetic where boolean expected" "if x then { skip } else { skip }";
  expect_fail "lexical error propagates" "x := 1 # 2"

let () = finish "parser"