open Printf

type directives = {
  debug: bool
}

let rec read_type_print dirs =
  print_string "# ";
  flush stdout;
  try
    let env = Syntax.Environment.empty in
    let e = Parser.toplevel Lexer.main @@ Lexing.from_channel stdin in
    begin match e with
    | Exp e ->
        let e, u, u_a, u_b = Typing.infer env e @@ Typing.fresh_tyvar () in
        if dirs.debug then begin
          prerr_endline "GSR:";
          prerr_endline @@ " e: " ^ Pp.GSR.string_of_exp e;
          prerr_endline @@ " U: " ^ Pp.string_of_type u;
          prerr_endline @@ " Uα: " ^ Pp.string_of_type u_a;
          prerr_endline @@ " Uβ: " ^ Pp.string_of_type u_b
        end;
        let f, u', u_a' = Typing.GSR.translate env e u_b in
        let v = Eval.eval f env (fun x -> x) in
        if dirs.debug then begin
          prerr_endline "CSR:";
          prerr_endline @@ " f: " ^ Pp.CSR.string_of_exp f;
          prerr_endline @@ " U: " ^ Pp.string_of_type u';
          prerr_endline @@ " Uα: " ^ Pp.string_of_type u_a';
          prerr_endline @@ " Uβ: " ^ Pp.string_of_type u_b
        end;
        print_endline @@ sprintf "- : %s = %s" (Pp.string_of_type u) (Pp.CSR.string_of_value v);
        read_type_print dirs
    | Directive d ->
        begin match d with
          | BoolDir ("debug", b) ->
              read_type_print { debug = b }
          | _ ->
              prerr_endline "unsupported directive";
              read_type_print dirs
        end
    end
(*
    TODO: cannot compare directly?
    assert (u = u');
    assert (u_a = u_a');
*)
  with
  | Failure message ->
      prerr_endline @@ sprintf "Failure: %s" message;
      read_type_print dirs
  | Parser.Error -> (* Menhir *)
      prerr_endline @@ sprintf "Parser.Error";
      read_type_print dirs
  | Typing.Type_error message ->
      prerr_endline @@ sprintf "Type_error: %s" message;
      read_type_print dirs
  | Eval.Eval_error message ->
      prerr_endline @@ sprintf "Eval_error: %s" message;
      read_type_print dirs

let () = read_type_print { debug = false }
