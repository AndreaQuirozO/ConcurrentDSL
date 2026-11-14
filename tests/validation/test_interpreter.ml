open Dsl
open Interpreter
open Helpers

let config src =
  make_configuration (Parser.program Lexer.token (Lexing.from_string src))

let outcomes src = explore (config src)

(* exact set of (status, final heap) over all interleavings *)
let expect name src expected =
  check name (try outcomes src = expected with _ -> false)

(* exact set of statuses over all interleavings *)
let expect_status name src expected =
  check name (try statuses (config src) = expected with _ -> false)

(* a particular outcome is reachable in some interleaving *)
let reachable name src o =
  check name (try List.mem o (outcomes src) with _ -> false)

let faults name src =
  check name (try ignore (outcomes src); false with Runtime_error _ -> true)

(* ---------- sequential core ---------- *)
let () =
  expect "assign" "x := 1; y := x + 2"
    [ (Terminated, [ ("x", 1); ("y", 3) ]) ];
  expect "reassign updates the same cell" "x := 1; x := x + 1"
    [ (Terminated, [ ("x", 2) ]) ];
  expect "while loop" "x := 0; while x < 3 do { x := x + 1 }"
    [ (Terminated, [ ("x", 3) ]) ];
  expect "while: body never runs" "x := 5; while x < 3 do { x := 0 }"
    [ (Terminated, [ ("x", 5) ]) ];
  expect "if: then branch" "x := 5; if x > 3 then { y := 1 } else { y := 0 }"
    [ (Terminated, [ ("x", 5); ("y", 1) ]) ];
  expect "if: else branch" "x := 1; if x > 3 then { y := 1 } else { y := 0 }"
    [ (Terminated, [ ("x", 1); ("y", 0) ]) ];
  expect "boolean connectives"
    "x := 2; if x > 1 and not (x = 5) or false then { y := 1 } else { y := 0 }"
    [ (Terminated, [ ("x", 2); ("y", 1) ]) ];
  expect "empty program body" "skip" [ (Terminated, []) ];
  expect "infinite loop hits the depth bound"
    "while true do { skip }" [ (OutOfFuel, []) ]

(* ---------- threads ---------- *)
let () =
  expect "spawn + join: child's write is visible"
    "x := 0; t := spawn { x := 5 }; join t"
    [ (Terminated, [ ("x", 5) ]) ];
  expect "join waits for an unfinished child"
    "x := 0; t := spawn { x := 1; x := x + 1 }; join t; y := x"
    [ (Terminated, [ ("x", 2); ("y", 2) ]) ];
  expect "race: both final values are possible"
    "x := 0; t := spawn { x := 1 }; x := 2; join t"
    [ (Terminated, [ ("x", 1) ]); (Terminated, [ ("x", 2) ]) ];
  expect "atomic increments commute"
    "x := 0; t := spawn { x := x + 1 }; x := x + 10; join t"
    [ (Terminated, [ ("x", 11) ]) ];
  expect "unjoined child still runs to completion"
    "x := 0; t := spawn { x := 7 }" [ (Terminated, [ ("x", 7) ]) ];
  expect "nested spawn"
    "x := 0; t := spawn { u := spawn { x := 3 }; join u; x := x + 1 }; join t"
    [ (Terminated, [ ("x", 4) ]) ];
  expect "two children"
    "x := 0; y := 0; t := spawn { x := 1 }; u := spawn { y := 2 }; join t; join u"
    [ (Terminated, [ ("x", 1); ("y", 2) ]) ]

(* ---------- locks and deadlock ---------- *)
let () =
  expect_status "two distinct locks can both be held"
    "a := create_lock(); b := create_lock(); lock a; lock b; unlock b; unlock a"
    [ Terminated ];
  expect_status "same lock order: never deadlocks"
    "a := create_lock(); b := create_lock(); \
     t := spawn { lock a; lock b; unlock b; unlock a }; \
     lock a; lock b; unlock b; unlock a; join t"
    [ Terminated ];
  expect_status "opposite lock order: some schedules deadlock, some don't"
    "a := create_lock(); b := create_lock(); \
     t := spawn { lock a; lock b; unlock b; unlock a }; \
     lock b; lock a; unlock a; unlock b; join t"
    [ Terminated; Deadlock ];
  expect_status "self-deadlock (non-reentrant lock)"
    "a := create_lock(); lock a; lock a" [ Deadlock ];
  expect_status "lock released then reacquired"
    "a := create_lock(); lock a; unlock a; lock a; unlock a" [ Terminated ];
  expect_status "mutual exclusion: second thread waits, then proceeds"
    "a := create_lock(); \
     t := spawn { lock a; unlock a }; lock a; unlock a; join t"
    [ Terminated ];
  expect_status "holding a lock while joining a thread that needs it (schedule-dependent)"
    "a := create_lock(); t := spawn { lock a; unlock a }; \
     lock a; join t; unlock a"
    [ Terminated; Deadlock ];
  expect_status "lock taken BEFORE spawn, then join: always deadlocks"
    "a := create_lock(); lock a; t := spawn { lock a; unlock a }; \
     join t; unlock a"
    [ Deadlock ];
  expect_status "join on a thread blocked forever is a deadlock"
    "a := create_lock(); lock a; t := spawn { lock a }; join t"
    [ Deadlock ];
  reachable "deadlock outcome carries the heap too"
    "x := 1; a := create_lock(); lock a; lock a"
    (Deadlock, [ ("x", 1) ])

(* ---------- runtime faults (ill-formed programs) ---------- *)
let () =
  faults "read of undefined variable" "x := y";
  faults "child's new variable is invisible to the parent"
    "t := spawn { y := 1 }; join t; x := y";
  faults "unlock without holding" "a := create_lock(); unlock a";
  faults "lock on undeclared lock" "lock a";
  faults "join of unbound thread variable" "join t";
  faults "joining twice" "t := spawn { skip }; join t; join t"

let () =
  if !failures > 0 then (Printf.printf "\n%d test(s) FAILED\n" !failures; exit 1)
  else print_endline "\nall interpreter tests passed"