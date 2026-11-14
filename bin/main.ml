open Dsl

let usage () =
  prerr_endline "usage: dsl [--check | --explore | --deadlocks] file.dsl";
  exit 2

let parse_file f =
  let ic = open_in f in
  let r =
    try Ok (Parser.program Lexer.token (Lexing.from_channel ic))
    with
    | Lexer.LexingError m -> Error m
    | Parsing.Parse_error -> Error "syntax error"
  in
  close_in ic; r

let possible_deadlock ast =
  List.mem Interpreter.Deadlock
    (Interpreter.statuses (Interpreter.make_configuration ast))

let () =
  Random.self_init ();
  let mode, file =
    match Array.to_list Sys.argv with
    | [ _; f ] -> (`Run, f)
    | [ _; "--check"; f ] -> (`Check, f)          (* type check only *)
    | [ _; "--explore"; f ] -> (`Explore, f)      (* type check, then all schedules *)
    | [ _; "--deadlocks"; f ] -> (`Deadlocks, f)  (* skip type check, all schedules *)
    | _ -> usage ()
  in
  let ast =
    match parse_file file with
    | Ok a -> a
    | Error m -> prerr_endline ("parse error: " ^ m); exit 2
  in
  try
    if mode = `Deadlocks then begin
      if possible_deadlock ast then (print_endline "deadlock possible"; exit 3)
      else print_endline "no deadlock in any schedule"
    end else begin
      (match Typechecker.check ast with
       | Error m -> prerr_endline ("type error: " ^ m); exit 1
       | Ok () -> ());
      match mode with
      | `Check -> print_endline "well typed"
      | `Explore ->
          if possible_deadlock ast then (print_endline "deadlock possible"; exit 3)
          else print_endline "well typed, no deadlock in any schedule"
      | _ ->
          let status, g =
            Interpreter.run ~pick:Interpreter.random
              (Interpreter.make_configuration ast) in
          List.iter (fun (x, v) -> Printf.printf "%s = %d\n" x v)
            (Interpreter.final_heap g);
          print_endline
            (match status with
             | Interpreter.Terminated -> "terminated"
             | Interpreter.Deadlock -> "DEADLOCK"
             | Interpreter.OutOfFuel -> "out of fuel (infinite loop?)")
    end
  with Interpreter.Runtime_error m -> prerr_endline ("runtime error: " ^ m); exit 1