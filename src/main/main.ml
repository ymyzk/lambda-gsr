open Printf
open Typing

let rec read_type_print () =
  print_string "# ";
  flush stdout;
  try
    let env = Syntax.Environment.empty in
    let e = Parser.toplevel Lexer.main @@ Lexing.from_channel stdin in
    let e, u, u_a, u_b = Typing.infer env e @@ Typing.fresh_tyvar () in
    print_endline @@ sprintf "- : %s" @@ Pp.string_of_type u;
    print_endline @@ Pp.string_of_type u_a;
    print_endline @@ Pp.string_of_type u_b;
    print_endline @@ Pp.GSR.string_of_exp e;
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
