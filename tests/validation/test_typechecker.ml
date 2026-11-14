open Dsl
open Helpers

let parse s = Parser.program Lexer.token (Lexing.from_string s)

let accepted src =
  match Typechecker.check (parse src) with Ok () -> true | Error _ -> false

let deadlocks src =
  List.mem Interpreter.Deadlock
    (Interpreter.statuses (Interpreter.make_configuration (parse src)))

(* well typed, and indeed no schedule deadlocks (soundness on this example) *)
let positive name src =
  check ("accept: " ^ name) (try accepted src with _ -> false);
  check ("sound, no deadlock in any schedule: " ^ name)
    (try not (deadlocks src) with _ -> false)

(* rejected, and some schedule really deadlocks *)
let negative name src =
  check ("reject: " ^ name) (try not (accepted src) with _ -> false);
  check ("really deadlocks: " ^ name) (try deadlocks src with _ -> false)

(* rejected although no schedule deadlocks (the system is conservative) *)
let conservative name src =
  check ("reject: " ^ name) (try not (accepted src) with _ -> false);
  check ("no deadlock anyway: " ^ name) (try not (deadlocks src) with _ -> false)

(* ill-formed programs: only the rejection is checked *)
let ill_typed name src =
  check ("reject: " ^ name) (try not (accepted src) with _ -> false)

let same_order = {|
  a := create_lock(); b := create_lock();
  t := spawn { lock a; lock b; unlock b; unlock a };
  lock a; lock b; unlock b; unlock a; join t |}

(* ---------- deadlock-free programs: must be accepted ---------- *)
let () =
  positive "same lock order in both threads" same_order;
  positive "shared counter under one lock"
    {| x := 0; a := create_lock();
       t := spawn { lock a; x := x + 1; unlock a };
       lock a; x := x + 10; unlock a; join t |};
  positive "lock, unlock, lock again"
    "a := create_lock(); lock a; unlock a; lock a; unlock a";
  positive "spawn and join without locks" "x := 0; t := spawn { x := 5 }; join t";
  positive "hold a lock while joining a thread that uses a higher lock"
    {| a := create_lock(); b := create_lock();
       lock a; t := spawn { lock b; unlock b }; join t; unlock a |};
  positive "lock inside a loop"
    "a := create_lock(); i := 0; while i < 3 do { lock a; i := i + 1; unlock a }";
  positive "nested spawn"
    {| a := create_lock();
       t := spawn { u := spawn { lock a; unlock a }; join u };
       lock a; unlock a; join t |}

(* ---------- programs that can deadlock: must be rejected ---------- *)
let () =
  negative "opposite lock order"
    {| a := create_lock(); b := create_lock();
       t := spawn { lock a; lock b; unlock b; unlock a };
       lock b; lock a; unlock a; unlock b; join t |};
  negative "self-deadlock" "a := create_lock(); lock a; lock a";
  negative "join while holding a lock the child needs (schedule-dependent)"
    {| a := create_lock();
       t := spawn { lock a; unlock a };
       lock a; join t; unlock a |};
  negative "lock taken before spawn, then join (always deadlocks)"
    {| a := create_lock(); lock a;
       t := spawn { lock a; unlock a }; join t; unlock a |};
  negative "child never releases the lock the parent waits for"
    "a := create_lock(); lock a; t := spawn { lock a }; join t"

(* ---------- rejected but safe: shows the system is conservative ---------- *)
let () =
  conservative "lock never released"
    "a := create_lock(); lock a";
  conservative "both orders, but in one thread"
    {| a := create_lock(); b := create_lock();
       lock a; lock b; unlock b; unlock a;
       lock b; lock a; unlock a; unlock b |}

(* ---------- ill-formed programs ---------- *)
let () =
  ill_typed "lock on an integer" "x := 1; lock x";
  ill_typed "read of undefined variable" "y := x";
  ill_typed "join of unbound thread" "join t";
  ill_typed "unlock without lock" "a := create_lock(); unlock a";
  ill_typed "variable used as int and lock" "x := 1; x := create_lock()";
  ill_typed "join inside a loop" "t := spawn { skip }; while true do { join t }";
  ill_typed "double join" "t := spawn { skip }; join t; join t"

(* ---------- inferred levels ---------- *)
let () =
  check "levels: thread < lock a < lock b in the same-order program"
    (match Typechecker.check_levels (parse same_order) with
     | Ok ls ->
         List.assoc "thread t" ls < List.assoc "lock a" ls
         && List.assoc "lock a" ls < List.assoc "lock b" ls
     | Error _ -> false);
  check "error message names the conflicting locks"
    (match Typechecker.check (parse "a := create_lock(); lock a; lock a") with
     | Error m -> contains m "lock a"
     | Ok () -> false)

let () = finish "typechecker"