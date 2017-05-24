open Constraints
open Format
open Syntax

let pp_sep ppf () = fprintf ppf ", "

(* binop -> string *)
let pp_print_binop ppf op =
  pp_print_string ppf begin
    match op with
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Equal -> "="
    | Gt -> ">"
    | Lt -> "<"
  end

let pp_print_basety ppf b =
  pp_print_string ppf begin
    match b with
    | TyBool -> "bool"
    | TyInt -> "int"
    | TyUnit -> "unit"
  end

(*
let pp_print_type ppf t =
  let params = ref [] in
  let string_of_typaram tp =
    let rec string_of_typaram i = function
      | [] -> params := !params @ [tp]; i
      | x :: _ when x = tp -> i
      | _ :: params -> string_of_typaram (i + 1) params
    in
    let i = string_of_typaram 0 !params in
    "'" ^ String.make 1 @@ char_of_int @@ (int_of_char 'a') + i
  in
  *)
let rec pp_print_type ppf = function
  | TyParam p -> fprintf ppf "'a%d" p
  (* | TyParam tp -> string_of_typaram tp *)
  | TyVar x -> fprintf ppf "'x%d" x
  | TyBase b -> pp_print_basety ppf b
  | TyFun (t1, t2, t3, t4) ->
      (* TODO: can be better *)
      let with_paren ppf t = match t with
        | TyFun _ -> fprintf ppf "(%a)" pp_print_type t
        | _ -> fprintf ppf "%a" pp_print_type t
      in
      fprintf ppf "%a/%a -> %a/%a"
        with_paren t1
        with_paren t2
        with_paren t3
        with_paren t4
  | TyDyn -> pp_print_string ppf "?"

let pp_print_const ppf = function
  | ConstBool b -> pp_print_bool ppf b
  | ConstInt i -> pp_print_int ppf i
  | ConstUnit -> pp_print_string ppf "()"

let pp_print_type_annot ppf (x, t) =
  fprintf ppf "(%s: %a)" x pp_print_type t

let pp_print_answer_type_annot ppf t =
  fprintf ppf "^%a" pp_print_type t

let pp_print_constr ppf = function
  | CEqual (u1, u2) -> fprintf ppf "%a=%a" pp_print_type u1 pp_print_type u2
  | CConsistent (u1, u2) -> fprintf ppf "%a~%a" pp_print_type u1 pp_print_type u2

let pp_print_constraints ppf c =
  pp_print_list pp_print_constr ppf (Constraints.to_list c) ~pp_sep:pp_sep

let pp_print_subst ppf (x, t) =
  fprintf ppf "x%a=%a" pp_print_int x pp_print_type t

let pp_print_substitutions ppf s =
  pp_print_list pp_print_subst ppf s ~pp_sep:pp_sep

module GSR = struct
  open GSR

  let rec pp_print_exp ppf = function
    (* TODO print typarms correctly *)
    | Var id -> pp_print_string ppf id
    | Const c -> pp_print_const ppf c
    | BinOp (op, e1, e2) ->
        fprintf ppf "%a %a %a"
          pp_print_exp e1
          pp_print_binop op
          pp_print_exp e2
    | Fun (g, x, x_t, e) ->
        fprintf ppf "fun%a %a -> %a"
          pp_print_answer_type_annot g
          pp_print_type_annot (x, x_t)
          pp_print_exp e
    | App (e1, e2) ->
        fprintf ppf "((%a) (%a))"
          pp_print_exp e1
          pp_print_exp e2
    | Shift (k, k_t, e) ->
        fprintf ppf "shift %a -> (%a)"
          pp_print_type_annot (k, k_t)
          pp_print_exp e
    | Reset (e, u) ->
        fprintf ppf "reset%a (%a)"
          pp_print_answer_type_annot u
          pp_print_exp e
    | If (e1, e2, e3) ->
        fprintf ppf "if %a then %a else %a"
          pp_print_exp e1
          pp_print_exp e2
          pp_print_exp e3
    | Consq (e1, e2) ->
        fprintf ppf "%a; %a"
          pp_print_exp e1
          pp_print_exp e2
end

module CSR = struct
  open CSR

  (* TODO print correctly *)
  let rec pp_print_exp ppf = function
    | Var id -> pp_print_string ppf id
    | Const c -> pp_print_const ppf c
    | BinOp (op, e1, e2) ->
        fprintf ppf "%a %a %a"
          pp_print_exp e1
          pp_print_binop op
          pp_print_exp e2
    | Fun (g, x, x_t, e) ->
        fprintf ppf "fun%a %a -> %a"
          pp_print_answer_type_annot g
          pp_print_type_annot (x, x_t)
          pp_print_exp e
    | App (e1, e2) ->
        fprintf ppf "((%a) (%a))"
          pp_print_exp e1
          pp_print_exp e2
    | Shift (k, k_t, e) ->
        fprintf ppf "shift %a -> (%a)"
          pp_print_type_annot (k, k_t)
          pp_print_exp e
    | Reset (e, u) ->
        fprintf ppf "reset%a (%a)"
          pp_print_answer_type_annot u
          pp_print_exp e
    | If (e1, e2, e3) ->
        fprintf ppf "if %a then %a else %a"
          pp_print_exp e1
          pp_print_exp e2
          pp_print_exp e3
    | Consq (e1, e2) ->
        fprintf ppf "%a; %a"
          pp_print_exp e1
          pp_print_exp e2
    | Cast (e, u1, u2) ->
        fprintf ppf "(%a : %a => %a)"
          pp_print_exp e
          pp_print_type u1
          pp_print_type u2

  let pp_print_tag ppf = function
    | P p -> fprintf ppf "'a%d" p
    | B -> pp_print_string ppf "bool"
    | I -> pp_print_string ppf "int"
    | U -> pp_print_string ppf "unit"
    | Ar -> pp_print_string ppf "?/? -> ?/?"

  let rec pp_print_value ppf = function
    | IntV i -> pp_print_int ppf i
    | BoolV b -> pp_print_bool ppf b
    | UnitV -> pp_print_string ppf "()"
    | FunV _ -> pp_print_string ppf "<fun>"
    | Tagged (t, v) -> fprintf ppf "%a : %a => ?" pp_print_value v pp_print_tag t
end
