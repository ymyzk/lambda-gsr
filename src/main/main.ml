open Format

type directives = {
  debug: bool
}

let rec read_eval_print lexeme dirs =
  let dirs = ref dirs in
  let read_eval_print = read_eval_print lexeme in
  fprintf std_formatter "# @?";
  flush stdout;
  ignore @@ flush_str_formatter ();
  begin try
    let env = Syntax.Environment.empty in
    let e = Parser.toplevel Lexer.main lexeme in
    let ppf = if !dirs.debug then std_formatter else str_formatter in
    begin match e with
    | Syntax.GSR.Exp e ->
        let u_b = Typing.GSR.fresh_tyvar () in
        fprintf ppf "Input:\n e: %a\n Uβ: %a\n"
          Pp.GSR.pp_print_exp e
          Pp.pp_print_type u_b;
        let e, u, u_a, u_b = Typing.GSR.infer env e u_b ~formatter:(if !dirs.debug then Some std_formatter else None) in
        fprintf ppf "GSR:\n e: %a\n U: %a\n Uα: %a\n Uβ: %a\n"
          Pp.GSR.pp_print_exp e
          Pp.pp_print_type u
          Pp.pp_print_type u_a
          Pp.pp_print_type u_b;
        let f, u', u_a' = Typing.GSR.translate env e u_b in
        (* Translation must not change types *)
        assert (u = u');
        assert (u_a = u_a');
        let u'', u_a'' = Typing.CSR.type_of_exp env f u_b in
        assert (u' = u'');
        assert (u_a' = u_a'');
        fprintf ppf "CSR:\n f: %a\n U: %a\n Uα: %a\n Uβ: %a\n"
          Pp.CSR.pp_print_exp f
          Pp.pp_print_type u'
          Pp.pp_print_type u_a'
          Pp.pp_print_type u_b;
        let v = Eval.eval f env (fun x -> x) in
        fprintf std_formatter "- : %a = %a\n"
          Pp.pp_print_type u
          Pp.CSR.pp_print_value v
    | Syntax.GSR.Directive d ->
        begin match d with
          | Syntax.GSR.BoolDir ("debug", b) ->
              prerr_endline @@ "debug mode " ^ if b then "enabled" else "disabled";
              dirs := { debug = b }
          | _ ->
              prerr_endline "unsupported directive"
        end
    end
  with
  | Failure message ->
      prerr_endline @@ Printf.sprintf "Failure: %s" message;
  | Parser.Error -> (* Menhir *)
      prerr_endline @@ Printf.sprintf "Parser.Error";
  | Typing.Type_error message ->
      prerr_endline @@ Printf.sprintf "Type_error: %s" message;
  | Typing.Unification_error (message, c) ->
      fprintf std_formatter ("Unification_error: " ^^ message ^^ "\n") Pp.pp_print_constr c;
  | Eval.Eval_error message ->
      prerr_endline @@ Printf.sprintf "Eval_error: %s" message;
  | Eval.Blame (value, message) ->
      fprintf std_formatter "Blame: %a => %s\n" Pp.CSR.pp_print_value value message;
  end;
  read_eval_print !dirs

let () =
  let lexeme = Lexing.from_channel stdin in
  read_eval_print lexeme { debug = false }
