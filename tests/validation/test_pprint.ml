open Dsl
open Ast
open Helpers

let parse s = Parser.program Lexer.token (Lexing.from_string s)

(* ---------- exact output ---------- *)
let () =
  let s c = Pprint.strCom (0, c) in
  check "print: assign" (s (Assign ("x", Plus (Int 1, Int 2))) = "x := (1 + 2)");
  check "print: seq" (s (Seq (Skip, Skip)) = "skip;\nskip");
  check "print: if"
    (s (If (True, Skip, Skip)) = "if true then {\n  skip\n} else {\n  skip\n}");
  check "print: while" (s (While (True, Skip)) = "while true do {\n  skip\n}");
  check "print: spawn"
    (s (Spawn ("t", Assign ("x", Int 1))) = "t := spawn {\n  x := 1\n}");
  check "print: join/lock/unlock/create_lock"
    (s (Join "t") = "join t" && s (Lock "a") = "lock a"
     && s (Unlock "a") = "unlock a" && s (CreateLock "a") = "a := create_lock()");
  check "print: not" (Pprint.strBexp (Not True) = "not true")

(* ---------- round-trip: parse (print (parse s)) = parse s ---------- *)
let roundtrip name src =
  check name
    (try
       let ast = parse src in
       parse (Pprint.strCom (0, ast)) = ast
     with _ -> false)

let () =
  roundtrip "roundtrip: arithmetic" "x := 1 + 2 * (3 - -4)";
  roundtrip "roundtrip: left-assoc minus" "x := 1 - 2 - 3";
  roundtrip "roundtrip: booleans"
    "if not (x < 1 or y = 2) and true then { skip } else { y := x }";
  roundtrip "roundtrip: all comparisons"
    "if x = 1 and x != 2 and x < 3 and x <= 4 and x > 5 and x >= 6 \
     then { skip } else { skip }";
  roundtrip "roundtrip: while" "while x < 3 do { x := x + 1; y := x }";
  roundtrip "roundtrip: empty bodies" "while false do { }; t := spawn { }";
  roundtrip "roundtrip: spawn/join/locks"
    "a := create_lock(); t := spawn { lock a; x := 1; unlock a }; join t";
  roundtrip "roundtrip: nested spawn"
    "t := spawn { u := spawn { skip }; join u }";
  roundtrip "roundtrip: full program"
    "a := create_lock(); b := create_lock(); x := 0; \
     t := spawn { lock a; lock b; x := x + 1; unlock b; unlock a }; \
     lock a; lock b; x := x + 10; unlock b; unlock a; join t"

let () = finish "printer"