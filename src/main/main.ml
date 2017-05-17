open Format

type directives = {
  debug: bool
}

let rec read_eval_print lexeme dirs =
  (* Used in all modes *)
  let ppf_std = std_formatter in
  (* Used in debug mode *)
  let ppf = if dirs.debug then std_formatter else str_formatter in
  let dirs = ref dirs in
  let read_eval_print = read_eval_print lexeme in
  fprintf ppf_std "# @?";
  flush stdout;
  ignore @@ flush_str_formatter ();
  begin try
    let env = Syntax.Environment.empty in
    let e = Parser.toplevel Lexer.main lexeme in
    begin match e with
    | Syntax.GSR.Exp e ->
        (* Inference *)
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
        if u_a <> u_b then begin
          fprintf ppf_std "Warning: This expression is not pure.\n";
          fprintf ppf_std "Answer types are %a and %a.\n"
            Pp.pp_print_type u_a
            Pp.pp_print_type u_b
        end;
        (* Translation *)
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
        (* Evaluation *)
        let v = Eval.eval f env (fun x -> x) in
        fprintf ppf_std "- : %a = %a\n"
          Pp.pp_print_type u
          Pp.CSR.pp_print_value v
    | Syntax.GSR.Directive d ->
        begin match d with
          | Syntax.GSR.BoolDir ("debug", b) ->
              fprintf ppf_std @@ "debug mode " ^^ (if b then "enabled" else "disabled") ^^ "\n";
              dirs := { debug = b }
          | _ ->
              fprintf ppf_std "unsupported directive"
        end
    end
  with
  | Failure message ->
      fprintf ppf_std "Failure: %s" message;
  (* Soft errors *)
  | Parser.Error -> (* Menhir *)
      fprintf ppf_std "Parser.Error";
  | Typing.Type_error message ->
      fprintf ppf "Type_error: %s" message;
  | Typing.Type_error1 (message, u1) ->
      fprintf ppf_std ("Type_error1: " ^^ message ^^ "\n")
        Pp.pp_print_type u1;
  | Typing.Type_error2 (message, u1, u2) ->
      fprintf ppf_std ("Type_error2: " ^^ message ^^ "\n")
        Pp.pp_print_type u1
        Pp.pp_print_type u2;
  | Typing.Unification_error (message, c) ->
      fprintf ppf_std ("Unification_error: " ^^ message ^^ "\n") Pp.pp_print_constr c;
  | Eval.Blame (value, message) ->
      fprintf ppf_std "Blame: %a => %s\n" Pp.CSR.pp_print_value value message;
  (* Fatal errors *)
  | Typing.Type_fatal_error message ->
      fprintf ppf_std "FATAL: Type_fatal_error: %s" message
  | Eval.Eval_fatal_error message ->
      fprintf ppf_std "FATAL: Eval_fatal_error: %s" message
  end;
  read_eval_print !dirs

let () =
  let lexeme = Lexing.from_channel stdin in
  read_eval_print lexeme { debug = false }
