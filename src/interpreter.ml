open Ast

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)
module StrMap = Map.Make (String)

(* The program is ill-formed at runtime (the type checker should later rule
   most of these out). Different from being blocked, which is normal . *)
exception Runtime_error of string

(* ---------- configurations ---------- *)

type store = int StrMap.t

(* γ = ⟨σI, σL, σT, λ, c⟩ *)
type loc = {
  si : store;          (* σI: int var  -> heap address *)
  sl : store;          (* σL: lock var -> lock id *)
  st : store;          (* σT: thread var -> thread id *)
  locked : IntSet.t;   (* λ: lock ids held by this thread *)
  c : com;
}

(* ⟨Π, h⟩ *)
type global = {
  pi : loc IntMap.t;   (* Π: thread id -> local configuration *)
  heap : int IntMap.t; (* h: address -> value *)
}

let main_id = 0

let make_configuration (c : com) : global =
  let main = { si = StrMap.empty; sl = StrMap.empty; st = StrMap.empty;
               locked = IntSet.empty; c } in
  { pi = IntMap.singleton main_id main; heap = IntMap.empty }

let terminated l = match l.c with Skip -> true | _ -> false

(* ---------- predicates from the rules ---------- *)

(* n ∉ dom(Π) *)
let fresh_thread g =
  let rec go n = if IntMap.mem n g.pi then go (n + 1) else n in
  go 0

(* fresh_lock(n): no thread's σL mentions n *)
let fresh_lock g =
  1 + IntMap.fold
        (fun _ l acc -> StrMap.fold (fun _ n acc -> max n acc) l.sl acc)
        g.pi (-1)

(* available_lock(n): no thread (including this one) holds n *)
let available_lock g n =
  IntMap.for_all (fun _ l -> not (IntSet.mem n l.locked)) g.pi

(* fresh heap address (the heap never shrinks, so its size is unused) *)
let fresh_addr g = IntMap.cardinal g.heap

(* ---------- expressions: ⟦a⟧(h ∘ σI) ---------- *)

let find name s x =
  match StrMap.find_opt x s with
  | Some n -> n
  | None -> raise (Runtime_error (Printf.sprintf "unbound %s variable %s" name x))

let rec evala g l a : int =
  match a with
  | Int n -> n
  | Var x -> IntMap.find (find "integer" l.si x) g.heap
  | Plus (a1, a2) -> evala g l a1 + evala g l a2
  | Minus (a1, a2) -> evala g l a1 - evala g l a2
  | Times (a1, a2) -> evala g l a1 * evala g l a2

let rec evalb g l b : bool =
  match b with
  | True -> true
  | False -> false
  | Equals (a1, a2) -> evala g l a1 = evala g l a2
  | NotEquals (a1, a2) -> evala g l a1 <> evala g l a2
  | Less (a1, a2) -> evala g l a1 < evala g l a2
  | LessEq (a1, a2) -> evala g l a1 <= evala g l a2
  | Greater (a1, a2) -> evala g l a1 > evala g l a2
  | GreaterEq (a1, a2) -> evala g l a1 >= evala g l a2
  | Not b -> not (evalb g l b)
  | And (b1, b2) -> evalb g l b1 && evalb g l b2
  | Or (b1, b2) -> evalb g l b1 || evalb g l b2

(* ---------- local step  ⟨Π,h,γ⟩ → ⟨Π',h',γ'⟩ ---------- *)

type result =
  | Stepped of global * loc   (* new Π', h'; and the new local config γ' *)
  | Blocked                   (* no rule applies right now (waiting, or skip) *)

let rec step (g : global) (l : loc) : result =
  match l.c with
  | Skip -> Blocked
  | Assign (x, a) ->
      let v = evala g l a in
      (match StrMap.find_opt x l.si with
       | Some addr ->
           Stepped ({ g with heap = IntMap.add addr v g.heap }, { l with c = Skip })
       | None ->
           let addr = fresh_addr g in
           Stepped ({ g with heap = IntMap.add addr v g.heap },
                    { l with si = StrMap.add x addr l.si; c = Skip }))
  | Seq (Skip, c2) -> Stepped (g, { l with c = c2 })
  | Seq (c1, c2) ->
      (match step g { l with c = c1 } with
       | Stepped (g', l') -> Stepped (g', { l' with c = Seq (l'.c, c2) })
       | Blocked -> Blocked)
  | If (b, c1, c2) ->
      Stepped (g, { l with c = (if evalb g l b then c1 else c2) })
  | While (b, c1) ->
      Stepped (g, { l with c = If (b, Seq (c1, l.c), Skip) })
  | Spawn (x, c1) ->
      let n = fresh_thread g in
      let child = { l with st = StrMap.empty; locked = IntSet.empty; c = c1 } in
      Stepped ({ g with pi = IntMap.add n child g.pi },
               { l with st = StrMap.add x n l.st; c = Skip })
  | Join x ->
      let n = find "thread" l.st x in
      (match IntMap.find_opt n g.pi with
       | Some child when terminated child ->
           Stepped ({ g with pi = IntMap.remove n g.pi },
                    { l with st = StrMap.remove x l.st; c = Skip })
       | Some _ -> Blocked
       | None -> raise (Runtime_error ("join: no such thread " ^ x)))
  | CreateLock x ->
      let n = fresh_lock g in
      Stepped (g, { l with sl = StrMap.add x n l.sl; c = Skip })
  | Lock x ->
      let n = find "lock" l.sl x in
      if available_lock g n
      then Stepped (g, { l with locked = IntSet.add n l.locked; c = Skip })
      else Blocked
  | Unlock x ->
      let n = find "lock" l.sl x in
      if IntSet.mem n l.locked
      then Stepped (g, { l with locked = IntSet.remove n l.locked; c = Skip })
      else raise (Runtime_error ("unlock of a lock not held: " ^ x))

(* ---------- global step  ⟨Π,h⟩ ⇒ ⟨Π'',h'⟩ ---------- *)

(* All possible next global configurations, tagged with the thread that moved.
   The GlobalStep rule is: pick any element of this list. *)
let successors (g : global) : (int * global) list =
  IntMap.fold
    (fun n l acc ->
       match step g l with
       | Stepped (g', l') -> (n, { g' with pi = IntMap.add n l' g'.pi }) :: acc
       | Blocked -> acc)
    g.pi []
  |> List.rev

(* ---------- observing results ---------- *)

(* Values of the main thread's integer variables, read through σI and h.
   Sorted by variable name. *)
let final_heap (g : global) : (var * int) list =
  let main = IntMap.find main_id g.pi in
  StrMap.bindings main.si
  |> List.map (fun (x, addr) -> (x, IntMap.find addr g.heap))

type status = Terminated | Deadlock | OutOfFuel

type outcome = status * (var * int) list

let all_done g = IntMap.for_all (fun _ l -> terminated l) g.pi

(* ---------- schedulers and drivers ---------- *)

type scheduler = (int * global) list -> int * global

let first : scheduler = List.hd
let random : scheduler = fun s -> List.nth s (Random.int (List.length s))

(* Run ONE schedule. Deadlock = unfinished threads but none can step. *)
let run ?(fuel = 100_000) ?(pick = first) (g : global) : status * global =
  let rec go fuel g =
    if all_done g then (Terminated, g)
    else if fuel = 0 then (OutOfFuel, g)
    else match successors g with
      | [] -> (Deadlock, g)
      | s -> go (fuel - 1) (snd (pick s))
  in
  go fuel g

(* Explore EVERY interleaving; returns the distinct outcomes.
   Exponential in the number of steps: use on small programs only. *)
let explore ?(depth = 200) (g : global) : outcome list =
  let rec go d g =
    if all_done g then [ (Terminated, final_heap g) ]
    else if d = 0 then [ (OutOfFuel, final_heap g) ]
    else match successors g with
      | [] -> [ (Deadlock, final_heap g) ]
      | s -> List.concat_map (fun (_, g') -> go (d - 1) g') s
  in
  List.sort_uniq compare (go depth g)

(* Just the set of statuses over all interleavings. *)
let statuses ?depth g : status list =
  List.sort_uniq compare (List.map fst (explore ?depth g))