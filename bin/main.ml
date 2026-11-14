open Dsl

let () =
  Random.self_init ();
  let ic = open_in Sys.argv.(1) in
  let ast = Parser.program Lexer.token (Lexing.from_channel ic) in
  close_in ic;
  try
    let status, g = Interpreter.run ~pick:Interpreter.random
        (Interpreter.make_configuration ast) in
    List.iter (fun (x, v) -> Printf.printf "%s = %d\n" x v)
      (Interpreter.final_heap g);
    match status with
    | Interpreter.Terminated -> print_endline "terminated"
    | Interpreter.Deadlock -> print_endline "DEADLOCK"
    | Interpreter.OutOfFuel -> print_endline "out of fuel (infinite loop?)"
  with Interpreter.Runtime_error m -> prerr_endline ("runtime error: " ^ m); exit 1