open Dsl
open Ast
open Interpreter
open Helpers

(* ---------- building configurations by hand ---------- *)
let mk_map of_seq l = of_seq (List.to_seq l)

let loc ?(si = []) ?(sl = []) ?(st = []) ?(locked = []) c =
  { si = mk_map StrMap.of_seq si; sl = mk_map StrMap.of_seq sl;
    st = mk_map StrMap.of_seq st; locked = IntSet.of_list locked; c }

let glob pi heap =
  { pi = mk_map IntMap.of_seq pi; heap = mk_map IntMap.of_seq heap }

(* Maps with equal bindings can have different tree shapes, so compare
   normalised forms instead of using (=) on the records. *)
let norm_loc l =
  (StrMap.bindings l.si, StrMap.bindings l.sl, StrMap.bindings l.st,
   IntSet.elements l.locked, l.c)
let norm g =
  (IntMap.bindings (IntMap.map norm_loc g.pi), IntMap.bindings g.heap)

(* Local step of thread n:  <Pi,h,Pi(n)> -> <Pi'',h',gamma'> *)
let step_of g n = step g (IntMap.find n g.pi)

let steps_to name g n (g_exp, l_exp) =
  check name
    (try match step_of g n with
       | Stepped (g', l') -> norm g' = norm g_exp && norm_loc l' = norm_loc l_exp
       | Blocked -> false
     with _ -> false)

let blocked name g n =
  check name (try step_of g n = Blocked with _ -> false)

let faults name g n =
  check name
    (try ignore (step_of g n); false with Runtime_error _ -> true)

let e = []  (* empty store / set *)

(* ---------- expressions: [[a]]_{h,sigma_I} = h(sigma_I(x)) ---------- *)
let () =
  let l = loc ~si:[ ("x", 0); ("y", 1) ] Skip in
  let g = glob [ (0, l) ] [ (0, 5); (1, 7) ] in
  check "eval: variable goes through sigma_I then h" (evala g l (Var "x") = 5);
  check "eval: literal" (evala g l (Int 9) = 9);
  check "eval: arithmetic"
    (evala g l (Plus (Var "x", Times (Var "y", Int 2))) = 19);
  check "eval: comparison" (evalb g l (Less (Var "x", Var "y")));
  check "eval: boolean connectives"
    (evalb g l (Or (False, And (Not False, True))));
  check "eval: aliasing, two vars on the same cell"
    (let l2 = loc ~si:[ ("x", 0); ("z", 0) ] Skip in
     evala g l2 (Var "z") = 5);
  check "eval: unbound variable is a fault"
    (try ignore (evala g l (Var "q")); false with Runtime_error _ -> true)

(* ---------- Skip ---------- *)
let () =
  blocked "Skip: no rule applies" (glob [ (0, loc Skip) ] []) 0

(* ---------- Assign ---------- *)
let () =
  let l = loc ~si:[ ("x", 0) ] (Assign ("x", Plus (Var "x", Int 1))) in
  let g = glob [ (0, l) ] [ (0, 5) ] in
  steps_to "AssignOld: updates the existing cell, store unchanged" g 0
    (glob [ (0, l) ] [ (0, 6) ], loc ~si:[ ("x", 0) ] Skip);

  let l = loc ~si:[ ("x", 0) ] (Assign ("y", Int 3)) in
  let g = glob [ (0, l) ] [ (0, 5) ] in
  steps_to "AssignNew: allocates a fresh cell and binds the variable" g 0
    (glob [ (0, l) ] [ (0, 5); (1, 3) ],
     loc ~si:[ ("x", 0); ("y", 1) ] Skip);

  (* a variable assigned by one thread is shared through the heap *)
  let l0 = loc ~si:[ ("x", 0) ] (Assign ("x", Int 9)) in
  let l1 = loc ~si:[ ("x", 0) ] Skip in
  let g = glob [ (0, l0); (1, l1) ] [ (0, 1) ] in
  steps_to "AssignOld: other threads' stores are untouched, cell shared" g 0
    (glob [ (0, l0); (1, l1) ] [ (0, 9) ], loc ~si:[ ("x", 0) ] Skip);

  faults "Assign: unbound variable on the right-hand side"
    (glob [ (0, loc (Assign ("x", Var "q"))) ] []) 0

(* ---------- Seq ---------- *)
let () =
  let c2 = Assign ("x", Int 1) in
  let l = loc (Seq (Skip, c2)) in
  steps_to "SeqSkip: skip; c steps to c, nothing else changes"
    (glob [ (0, l) ] []) 0
    (glob [ (0, l) ] [], loc c2);

  let l = loc ~si:[ ("x", 0) ] (Seq (Assign ("x", Int 4), Skip)) in
  steps_to "SeqStep: steps the first command, keeps the second"
    (glob [ (0, l) ] [ (0, 0) ]) 0
    (glob [ (0, l) ] [ (0, 4) ], loc ~si:[ ("x", 0) ] (Seq (Skip, Skip)));

  let l0 = loc ~sl:[ ("a", 0) ] (Seq (Lock "a", Skip)) in
  let l1 = loc ~locked:[ 0 ] Skip in
  blocked "SeqStep: blocked first command blocks the sequence"
    (glob [ (0, l0); (1, l1) ] []) 0

(* ---------- If / While ---------- *)
let () =
  let l = loc ~si:[ ("x", 0) ]
      (If (Less (Var "x", Int 3), Assign ("y", Int 1), Skip)) in
  let g n = glob [ (0, l) ] [ (0, n) ] in
  steps_to "IfTrue: steps to the then branch" (g 1) 0
    (g 1, loc ~si:[ ("x", 0) ] (Assign ("y", Int 1)));
  steps_to "IfFalse: steps to the else branch" (g 5) 0
    (g 5, loc ~si:[ ("x", 0) ] Skip);

  let w = While (Less (Var "x", Int 3), Assign ("x", Plus (Var "x", Int 1))) in
  let l = loc ~si:[ ("x", 0) ] w in
  steps_to "While: unfolds to if b then (c; while b do c) else skip"
    (glob [ (0, l) ] [ (0, 0) ]) 0
    (glob [ (0, l) ] [ (0, 0) ],
     loc ~si:[ ("x", 0) ]
       (If (Less (Var "x", Int 3),
            Seq (Assign ("x", Plus (Var "x", Int 1)), w), Skip)))

(* ---------- Spawn ---------- *)
let () =
  let body = Assign ("x", Int 1) in
  let parent = loc ~si:[ ("x", 0) ] ~sl:[ ("a", 0) ] ~locked:[ 0 ]
      (Spawn ("t", body)) in
  let g = glob [ (0, parent) ] [ (0, 0) ] in
  let child = loc ~si:[ ("x", 0) ] ~sl:[ ("a", 0) ] body in
  steps_to "Spawn: child gets sigma_I, sigma_L; empty sigma_T and lambda" g 0
    (glob [ (0, parent); (1, child) ] [ (0, 0) ],
     loc ~si:[ ("x", 0) ] ~sl:[ ("a", 0) ] ~st:[ ("t", 1) ] ~locked:[ 0 ] Skip);

  (* the implementation picks the smallest unused id (a valid instance of
     n notin dom(Pi)); this test pins that deterministic choice *)
  let other = loc Skip in
  let g = glob [ (0, parent); (1, other) ] [ (0, 0) ] in
  steps_to "Spawn: id is not in dom(Pi)" g 0
    (glob [ (0, parent); (1, other); (2, child) ] [ (0, 0) ],
     loc ~si:[ ("x", 0) ] ~sl:[ ("a", 0) ] ~st:[ ("t", 2) ] ~locked:[ 0 ] Skip);

  (* no premise on x: respawning into the same handle overwrites it *)
  let parent2 = loc ~st:[ ("t", 1) ] (Spawn ("t", Skip)) in
  let g = glob [ (0, parent2); (1, loc Skip) ] [] in
  steps_to "Spawn: re-using a handle overwrites the old binding" g 0
    (glob [ (0, parent2); (1, loc Skip); (2, loc Skip) ] [],
     loc ~st:[ ("t", 2) ] Skip)

(* ---------- Join ---------- *)
let () =
  let parent = loc ~st:[ ("t", 1) ] (Join "t") in
  let g = glob [ (0, parent); (1, loc Skip) ] [] in
  steps_to "Join: terminated child is removed from Pi and sigma_T" g 0
    (glob [ (0, parent) ] [], loc Skip);

  blocked "Join: child not terminated, no rule applies"
    (glob [ (0, parent); (1, loc (Assign ("x", Int 1))) ] []) 0;
  blocked "Join: child terminated is required, even if it is blocked itself"
    (glob [ (0, parent); (1, loc (Lock "a")) ] []) 0;
  faults "Join: thread id not in dom(Pi)"
    (glob [ (0, loc ~st:[ ("t", 7) ] (Join "t")) ] []) 0;
  faults "Join: unbound thread variable"
    (glob [ (0, loc (Join "t")) ] []) 0

(* ---------- CreateLock ---------- *)
let () =
  let l = loc (CreateLock "a") in
  steps_to "CreateLock: first lock, fresh id bound in sigma_L"
    (glob [ (0, l) ] []) 0
    (glob [ (0, l) ] [], loc ~sl:[ ("a", 0) ] Skip);

  (* fresh w.r.t. every thread's sigma_L, not just the creator's *)
  let other = loc ~sl:[ ("b", 3) ] Skip in
  steps_to "CreateLock: id is fresh w.r.t. all threads"
    (glob [ (0, l); (1, other) ] []) 0
    (glob [ (0, l); (1, other) ] [], loc ~sl:[ ("a", 4) ] Skip);

  let l2 = loc ~sl:[ ("a", 0) ] (CreateLock "b") in
  steps_to "CreateLock: second lock gets a different id"
    (glob [ (0, l2) ] []) 0
    (glob [ (0, l2) ] [], loc ~sl:[ ("a", 0); ("b", 1) ] Skip)

(* ---------- Lock ---------- *)
let () =
  let l = loc ~sl:[ ("a", 0) ] (Lock "a") in
  steps_to "Lock: available lock is added to lambda"
    (glob [ (0, l) ] []) 0
    (glob [ (0, l) ] [], loc ~sl:[ ("a", 0) ] ~locked:[ 0 ] Skip);

  steps_to "Lock: a lock held by nobody, other locks held elsewhere"
    (glob [ (0, l); (1, loc ~locked:[ 5 ] Skip) ] []) 0
    (glob [ (0, l); (1, loc ~locked:[ 5 ] Skip) ] [],
     loc ~sl:[ ("a", 0) ] ~locked:[ 0 ] Skip);

  blocked "Lock: held by another thread"
    (glob [ (0, l); (1, loc ~locked:[ 0 ] Skip) ] []) 0;
  blocked "Lock: held by the thread itself (non-reentrant)"
    (glob [ (0, loc ~sl:[ ("a", 0) ] ~locked:[ 0 ] (Lock "a")) ] []) 0;
  faults "Lock: unbound lock variable"
    (glob [ (0, loc (Lock "a")) ] []) 0

(* ---------- Unlock ---------- *)
let () =
  let l = loc ~sl:[ ("a", 0) ] ~locked:[ 0; 1 ] (Unlock "a") in
  steps_to "Unlock: held lock is removed from lambda, others stay"
    (glob [ (0, l) ] []) 0
    (glob [ (0, l) ] [], loc ~sl:[ ("a", 0) ] ~locked:[ 1 ] Skip);

  faults "Unlock: lock not held by anyone"
    (glob [ (0, loc ~sl:[ ("a", 0) ] (Unlock "a")) ] []) 0;
  faults "Unlock: lock held by a different thread"
    (glob [ (0, loc ~sl:[ ("a", 0) ] (Unlock "a"));
            (1, loc ~locked:[ 0 ] Skip) ] []) 0;
  faults "Unlock: unbound lock variable"
    (glob [ (0, loc (Unlock "a")) ] []) 0

(* ---------- GlobalStep: any enabled thread may move ---------- *)
let () =
  let assign v = loc ~si:[ ("x", 0) ] (Assign ("x", Int v)) in
  let g = glob [ (0, assign 1); (1, assign 2) ] [ (0, 0) ] in
  let succs = List.sort compare
      (List.map (fun (n, g') -> (n, norm g')) (successors g)) in
  let moved n v other =
    (n, norm (glob [ (n, loc ~si:[ ("x", 0) ] Skip); (1 - n, other) ]
                [ (0, v) ])) in
  check "GlobalStep: one successor per enabled thread"
    (succs = List.sort compare [ moved 0 1 (assign 2); moved 1 2 (assign 1) ]);

  let l = loc ~sl:[ ("a", 0) ] (Lock "a") in
  let g = glob [ (0, l); (1, loc ~locked:[ 0 ] (Skip)) ] [] in
  check "GlobalStep: blocked and terminated threads yield no successor"
    (successors g = []);

  let g = glob [ (0, loc ~sl:[ ("a", 0) ] (Lock "a"));
                 (1, loc ~locked:[ 0 ] (Assign ("y", Int 1))) ] [] in
  check "GlobalStep: only the enabled thread moves"
    (List.map fst (successors g) = [ 1 ])

let () = finish "step"