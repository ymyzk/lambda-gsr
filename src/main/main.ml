open Format

type directives = {
  debug: bool
}

let rec read_eval_print lexeme dirs =
  let read_eval_print = read_eval_print lexeme in
  fprintf std_formatter "# @?";
  flush stdout;
  try
    let env = Syntax.Environment.empty in
    let e = Parser.toplevel Lexer.main lexeme in
    begin match e with
    | Syntax.GSR.Exp e ->
        let u_b = Typing.GSR.fresh_tyvar () in
        if dirs.debug then begin
          prerr_endline "Input:";
          prerr_endline @@ " e: " ^ Pp.GSR.string_of_exp e;
          prerr_endline @@ " Uβ: " ^ Pp.string_of_type u_b
        end;
        let e, u, u_a, u_b = Typing.GSR.infer env e u_b ~formatter:(if dirs.debug then Some std_formatter else None) in
        if dirs.debug then begin
          prerr_endline "GSR:";
          prerr_endline @@ " e: " ^ Pp.GSR.string_of_exp e;
          prerr_endline @@ " U: " ^ Pp.string_of_type u;
          prerr_endline @@ " Uα: " ^ Pp.string_of_type u_a;
          prerr_endline @@ " Uβ: " ^ Pp.string_of_type u_b
        end;
        let f, u', u_a' = Typing.GSR.translate env e u_b in
        (* Translation must not change types *)
        assert (u = u');
        assert (u_a = u_a');
        let u'', u_a'' = Typing.CSR.type_of_exp env f u_b in
        assert (u' = u'');
        assert (u_a' = u_a'');
        if dirs.debug then begin
          prerr_endline "CSR:";
          prerr_endline @@ " f: " ^ Pp.CSR.string_of_exp f;
          prerr_endline @@ " U: " ^ Pp.string_of_type u';
          prerr_endline @@ " Uα: " ^ Pp.string_of_type u_a';
          prerr_endline @@ " Uβ: " ^ Pp.string_of_type u_b
        end;
        let v = Eval.eval f env (fun x -> x) in
        fprintf std_formatter "- : %s = %a\n" (Pp.string_of_type u) Pp.CSR.pp_print_value v;
        read_eval_print dirs
    | Syntax.GSR.Directive d ->
        begin match d with
          | Syntax.GSR.BoolDir ("debug", b) ->
              prerr_endline @@ "debug mode " ^ if b then "enabled" else "disabled";
              read_eval_print { debug = b }
          | _ ->
              prerr_endline "unsupported directive";
              read_eval_print dirs
        end
    end
  with
  | Failure message ->
      prerr_endline @@ Printf.sprintf "Failure: %s" message;
      read_eval_print dirs
  | Parser.Error -> (* Menhir *)
      prerr_endline @@ Printf.sprintf "Parser.Error";
      read_eval_print dirs
  | Typing.Type_error message ->
      prerr_endline @@ Printf.sprintf "Type_error: %s" message;
      read_eval_print dirs
  | Typing.Unification_error (message, c) ->
      fprintf std_formatter ("Unification_error: " ^^ message ^^ "\n") Pp.pp_print_constr c;
      read_eval_print dirs
  | Eval.Eval_error message ->
      prerr_endline @@ Printf.sprintf "Eval_error: %s" message;
      read_eval_print dirs
  | Eval.Blame (value, message) ->
      fprintf std_formatter "Blame: %a => %s\n" Pp.CSR.pp_print_value value message;
      read_eval_print dirs

let () =
  let lexeme = Lexing.from_channel stdin in
  read_eval_print lexeme { debug = false }
