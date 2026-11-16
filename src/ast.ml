type var = string

type aexp =
  | Int of int
  | Var of var
  | Plus of aexp * aexp
  | Minus of aexp * aexp
  | Times of aexp * aexp

type bexp =
  | True
  | False
  | Equals of aexp * aexp
  | NotEquals of aexp * aexp
  | Less of aexp * aexp
  | LessEq of aexp * aexp
  | Greater of aexp * aexp
  | GreaterEq of aexp * aexp
  | Not of bexp
  | And of bexp * bexp
  | Or of bexp * bexp

type com =
  | Skip
  | Assign of var * aexp
  | Seq of com * com
  | If of bexp * com * com
  | While of bexp * com
  | Spawn of var * com      (* x := spawn { c } *)
  | Join of var             (* join x *)
  | CreateLock of var       (* x := create_lock() *)
  | Lock of var             (* lock x *)
  | Unlock of var           (* unlock x *)


type ty = 
  | TInt            (* int *)
  | TLock of int    (* lock(lev) *)
  | TTh of int      (* th(lev) *)
