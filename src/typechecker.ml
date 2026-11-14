(* Deadlock-freedom type system.  Judgment:  Γ, H, base ⊢ c ⇒ Γ', H'
   Levels are not written in programs: every lock / thread declaration gets a
   fresh level variable, the rules emit constraints  level(a) < level(b),
   and the program is well typed iff these constraints have no cycle. *)

open Ast

module SMap = Map.Make (String)
module SSet = Set.Make (String)

(* τ ::= int | lock(lev) | th(lev); the int is a level variable *)
type ty = TInt | TLock of int | TTh of int
type env = ty SMap.t

(* minimum level that may be used: 0, or (level of thread t) + 1 *)
type base = Zero | Above of int

exception Type_error of string
let err fmt = Printf.ksprintf (fun s -> raise (Type_error s)) fmt

type state = {
  mutable next : int;                        (* next level variable *)
  mutable decls : (int * string) list;       (* level var -> "lock a" / "thread t" *)
  mutable edges : (int * int * string) list; (* (a, b, why):  level a < level b *)
}

let fresh st descr =
  let i = st.next in
  st.next <- i + 1;
  st.decls <- (i, descr) :: st.decls;
  i

let descr st i = List.assoc i st.decls
let less st a b why = st.edges <- (a, b, why) :: st.edges

(* ---------- well-formed expressions: wf_a, wf_b ---------- *)

let rec wfa (g : env) a =
  match a with
  | Int _ -> ()
  | Var x ->
      (match SMap.find_opt x g with
       | Some TInt -> ()
       | Some _ -> err "%s is used as an integer but is not one" x
       | None -> err "integer variable %s is not defined" x)
  | Plus (a1, a2) | Minus (a1, a2) | Times (a1, a2) -> wfa g a1; wfa g a2

let rec wfb (g : env) b =
  match b with
  | True | False -> ()
  | Not b1 -> wfb g b1
  | And (b1, b2) | Or (b1, b2) -> wfb g b1; wfb g b2
  | Equals (a1, a2) | NotEquals (a1, a2) | Less (a1, a2) | LessEq (a1, a2)
  | Greater (a1, a2) | GreaterEq (a1, a2) -> wfa g a1; wfa g a2

(* ---------- constraints for  lev >= base  and  compatible_ob(Γ,H,lev) ---------- *)

let require_above st base l what =
  match base with
  | Zero -> ()
  | Above t ->
      less st t l (Printf.sprintf "%s is used inside the body of %s" what (descr st t))

let held_below st (g : env) h l what =
  SSet.iter
    (fun y ->
       match SMap.find_opt y g with
       | Some (TLock l') ->
           less st l' l (Printf.sprintf "%s is held while %s" (descr st l') what)
       | _ -> ())
    h

(* ---------- the judgment ---------- *)

let rec com st (g : env) (h : SSet.t) (base : base) (c : com) : env * SSet.t =
  match c with
  | Skip -> (g, h)
  | Assign (x, a) ->
      wfa g a;
      (match SMap.find_opt x g with
       | None | Some TInt -> (SMap.add x TInt g, h)
       | Some _ -> err "%s is assigned an integer but is not an integer variable" x)
  | Seq (c1, c2) ->
      let g1, h1 = com st g h base c1 in
      com st g1 h1 base c2
  | If (b, c1, c2) ->
      wfb g b;
      let g1, h1 = com st g h base c1 in
      let g2, h2 = com st g h base c2 in
      if not (SSet.equal h1 h2) then
        err "the two branches of an if hold different sets of locks";
      SSet.iter
        (fun x ->
           match SMap.find_opt x g with
           | Some (TLock _) -> ()
           | _ -> err "lock %s is acquired in a branch but not in scope after the if" x)
        h1;
      let keep x t = SMap.find_opt x g1 = Some t && SMap.find_opt x g2 = Some t in
      (SMap.filter keep g, h1)
  | While (b, body) ->
      wfb g b;
      let g1, h1 = com st g h base body in
      if not (SSet.equal h1 h) then
        err "a while body must leave the set of held locks unchanged";
      if not (SMap.for_all (fun x t -> SMap.find_opt x g1 = Some t) g) then
        err "a while body must not join threads";
      (g, h)
  | Spawn (x, body) ->
      if SMap.mem x g then err "thread variable %s is already defined" x;
      let t = fresh st ("thread " ^ x) in
      let child_env = SMap.filter (fun _ ty -> match ty with TTh _ -> false | _ -> true) g in
      let _, hc = com st child_env SSet.empty (Above t) body in
      if not (SSet.is_empty hc) then
        err "thread %s ends while still holding locks: %s" x
          (String.concat ", " (SSet.elements hc));
      (SMap.add x (TTh t) g, h)
  | Join x ->
      (match SMap.find_opt x g with
       | Some (TTh t) ->
           let what = "joining " ^ descr st t in
           require_above st base t what;
           held_below st g h t what;
           (SMap.remove x g, h)
       | _ -> err "join %s: not a thread handle in scope (unbound, already joined, or not a thread)" x)
  | CreateLock x ->
      if SMap.mem x g then err "lock variable %s is already defined" x;
      let l = fresh st ("lock " ^ x) in
      (SMap.add x (TLock l) g, h)
  | Lock x ->
      (match SMap.find_opt x g with
       | Some (TLock l) ->
           let what = "acquiring " ^ descr st l in
           require_above st base l what;
           held_below st g h l what;
           (g, SSet.add x h)
       | _ -> err "lock %s: not a lock in scope" x)
  | Unlock x ->
      (match SMap.find_opt x g with
       | Some (TLock _) ->
           if not (SSet.mem x h) then err "unlock %s: the lock is not held" x;
           (g, SSet.remove x h)
       | _ -> err "unlock %s: not a lock in scope" x)

(* ---------- solving the level constraints ---------- *)

let rec take_until b = function
  | [] -> []
  | ((a, _, _) as e) :: rest -> if a = b then [ e ] else e :: take_until b rest

let find_cycle st =
  let n = st.next in
  let color = Array.make (max n 1) 0 in (* 0 unvisited, 1 on stack, 2 done *)
  let result = ref None in
  let rec dfs path v =
    color.(v) <- 1;
    List.iter
      (fun ((a, b, _) as e) ->
         if a = v && !result = None then begin
           if color.(b) = 1 then
             result := Some (if a = b then [ e ] else e :: take_until b path)
           else if color.(b) = 0 then dfs (e :: path) b
         end)
      st.edges;
    color.(v) <- 2
  in
  for v = 0 to n - 1 do
    if color.(v) = 0 && !result = None then dfs [] v
  done;
  !result

(* longest-path levels; only called when the graph is acyclic *)
let assign_levels st =
  let lv = Array.make (max st.next 1) 0 in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun (a, b, _) ->
         if lv.(b) <= lv.(a) then (lv.(b) <- lv.(a) + 1; changed := true))
      st.edges
  done;
  lv

(* ---------- entry points ---------- *)

(* ∅, ∅, 0 ⊢ c ⇒ Γ, ∅ .  On success returns one valid level assignment,
   as (declaration, level) pairs in program order. *)
let check_levels (c : com) : ((string * int) list, string) result =
  let st = { next = 0; decls = []; edges = [] } in
  try
    let _, h = com st SMap.empty SSet.empty Zero c in
    match find_cycle st with
    | Some cyc ->
        let show (a, b, why) =
          Printf.sprintf "level(%s) < level(%s) because %s" (descr st a) (descr st b) why
        in
        err "no consistent lock order: %s" (String.concat "; " (List.rev_map show cyc))
    | None ->
        if not (SSet.is_empty h) then
          err "locks still held at the end of the program: %s"
            (String.concat ", " (SSet.elements h));
        let lv = assign_levels st in
        Ok (List.rev_map (fun (i, d) -> (d, lv.(i))) st.decls)
  with Type_error m -> Error m

let check (c : com) : (unit, string) result =
  Result.map (fun _ -> ()) (check_levels c)