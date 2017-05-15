open Constraints
open Format
open Syntax

let pp_sep ppf () = fprintf ppf ", "

(* binop -> string *)
let string_of_binop = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "="
  | Gt -> ">"
  | Lt -> "<"

let string_of_base_type = function
  | TyBool -> "bool"
  | TyInt -> "int"
  | TyUnit -> "unit"

let string_of_type t =
  (*
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
  let rec string_of_type = function
    | TyParam p -> "'a" ^ string_of_int p
    (* | TyParam tp -> string_of_typaram tp *)
    | TyVar x -> "'x" ^ string_of_int x
    | TyBase b -> string_of_base_type b
    | TyFun (t1, t2, t3, t4) ->
        let s1 = Printf.sprintf (match t1 with TyFun _ -> "(%s)" | _ -> "%s") @@ string_of_type t1 in
        let s2 = Printf.sprintf (match t2 with TyFun _ -> "(%s)" | _ -> "%s") @@ string_of_type t2 in
        let s3 = Printf.sprintf (match t3 with TyFun _ -> "(%s)" | _ -> "%s") @@ string_of_type t3 in
        let s4 = Printf.sprintf (match t4 with TyFun _ -> "(%s)" | _ -> "%s") @@ string_of_type t4 in
        Printf.sprintf "%s/%s -> %s/%s" s1 s2 s3 s4
    | TyDyn -> "?"
  in
  string_of_type t

let string_of_const = function
  | ConstBool b -> string_of_bool b
  | ConstInt i -> string_of_int i
  | ConstUnit -> "()"

(* string -> ty -> string *)
let string_of_type_annot x t = Printf.sprintf "(%s: %s)" x @@ string_of_type t

(* ty -> string *)
let string_of_answer_type_annot t = Printf.sprintf "^%s" @@ string_of_type t

let pp_print_constr ppf = function
  | CEqual (u1, u2) -> fprintf ppf "%s=%s" (string_of_type u1) (string_of_type u2)
  | CConsistent (u1, u2) -> fprintf ppf "%s~%s" (string_of_type u1) (string_of_type u2)

let pp_print_constraints ppf c =
  pp_print_list pp_print_constr ppf (Constraints.to_list c) ~pp_sep:pp_sep

let string_of_substitution (x, t) =
  Printf.sprintf "x%d=%s" x @@ string_of_type t

let string_of_substitutions s =
  String.concat ", " @@ List.map string_of_substitution s

module GSR = struct
  open GSR

  (* exp -> string *)
  let rec string_of_exp = function
    (* TODO print typarms correctly *)
    | Var id -> id
    | Const c -> string_of_const c
    | BinOp (op, e1, e2) ->
        Printf.sprintf "%s %s %s" (string_of_exp e1) (string_of_binop op) (string_of_exp e2)
    | Fun (g, x, x_t, e) ->
        Printf.sprintf "fun%s %s -> %s" (string_of_answer_type_annot g) (string_of_type_annot x x_t) (string_of_exp e)
    | App (x, y) -> Printf.sprintf "((%s) (%s))" (string_of_exp x) (string_of_exp y)
    | Shift (k, k_t, e) ->
        Printf.sprintf "shift %s -> (%s)" (string_of_type_annot k k_t) (string_of_exp e)
    | Reset (e, u) ->
        Printf.sprintf "reset%s (%s)" (string_of_answer_type_annot u) (string_of_exp e)
    | If (e1, e2, e3) ->
        Printf.sprintf "if %s then %s else %s" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
    | Consq (e1, e2) ->
        Printf.sprintf "%s; %s" (string_of_exp e1) (string_of_exp e2)
end

module CSR = struct
  open CSR
  open Eval

  (* TODO print correctly *)
  let rec string_of_exp = function
    | Var id -> id
    | Const c -> string_of_const c
    | BinOp (op, e1, e2) ->
        Printf.sprintf "%s %s %s" (string_of_exp e1) (string_of_binop op) (string_of_exp e2)
    | Fun (g, x, x_t, e) ->
        Printf.sprintf "fun%s %s -> %s" (string_of_answer_type_annot g) (string_of_type_annot x x_t) (string_of_exp e)
    | App (x, y) -> Printf.sprintf "(%s) (%s)" (string_of_exp x) (string_of_exp y)
    | Shift (k, k_t, e) ->
        Printf.sprintf "shift %s -> (%s)" (string_of_type_annot k k_t) (string_of_exp e)
    | Reset (e, u) ->
        Printf.sprintf "reset%s (%s)" (string_of_answer_type_annot u) (string_of_exp e)
    | If (e1, e2, e3) ->
        Printf.sprintf "if %s then %s else %s" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
    | Consq (e1, e2) ->
        Printf.sprintf "%s; %s" (string_of_exp e1) (string_of_exp e2)
    | Cast (e, u1, u2) ->
        Printf.sprintf "(%s : %s => %s)" (string_of_exp e) (string_of_type u1) (string_of_type u2)

  let string_of_tag = function
    | P p -> "'a" ^ string_of_int p
    | B -> "bool"
    | I -> "int"
    | U -> "unit"
    | Ar -> "*/* -> */*"

  let rec string_of_value = function
    | IntV i -> string_of_int i
    | BoolV b -> string_of_bool b
    | UnitV -> "()"
    | FunV _ -> "<fun>"
    | Tagged (t, v) -> Printf.sprintf "%s : %s => ?" (string_of_value v) (string_of_tag t)
end
