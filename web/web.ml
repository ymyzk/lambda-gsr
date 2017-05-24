let _ =
  Js.export "GsrInfer" @@
    object%js
      method infer x = begin
(*         let empty = Syntax.Environment.empty in *)
        try
          let x = Printf.sprintf "%s;;" @@ Js.to_string x in
          let e = Parser.toplevel Lexer.main @@ Lexing.from_string x in
          begin match e with
(*
          | Syntax.GSR.Exp e ->
            let _, u, _, _ = Typing.GSR.infer empty e @@ Typing.GSR.fresh_tyvar () in
            let s = Format.fprintf Format.str_formatter "%a" Pp.pp_print_type u; Format.flush_str_formatter () in
            object%js
              val isSucceeded = Js._true
              val result = Js.string s
            end
*)
          | _ ->
            object%js
              val isSucceeded = Js._false
              val result = Js.string "unsupported"
            end
          end
        with
        | Failure message ->
            object%js
              val isSucceeded = Js._false
              val result = Js.string @@ Printf.sprintf "Failure: %s" message
            end
        | Parser.Error -> (* Menhir *)
            object%js
              val isSucceeded = Js._false
              val result = Js.string "Parser.Error"
            end
        | Typing.Type_error message ->
            object%js
              val isSucceeded = Js._false
              val result = Js.string @@ Printf.sprintf "Type_error: %s" message
            end
        | _ ->
            object%js
              val isSucceeded = Js._false
              val result = Js.string "Unexpected error"
            end
      end
    end
