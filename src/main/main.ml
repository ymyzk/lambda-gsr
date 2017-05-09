open Printf
open Typing

let rec read_type_print () =
  print_string "# ";
  flush stdout;
  try
    let env = Syntax.Environment.empty in
    let e = Parser.toplevel Lexer.main @@ Lexing.from_channel stdin in
    let e, u, u_a, u_b = Typing.infer env e @@ Typing.fresh_tyvar () in
    let f, u', u_a' = Typing.GSR.translate env e u_b in
    let v = Eval.eval f env (fun x -> x) in
    print_endline @@ sprintf "- : %s = %s" (Pp.string_of_type u) (Pp.CSR.string_of_value v);
(*
    TODO: cannot compare directly?
    assert (u = u');
    assert (u_a = u_a');
*)
    read_type_print ()
  with
  | Failure message ->
      prerr_endline @@ sprintf "Failure: %s" message;
      read_type_print ()
  | Parser.Error -> (* Menhir *)
      prerr_endline @@ sprintf "Parser.Error";
      read_type_print ()
  | Type_error message ->
      prerr_endline @@ sprintf "Type_error: %s" message;
      read_type_print ()

let () = read_type_print ()
