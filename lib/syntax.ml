(* Types *)

type typaram = int
type tyvar = int

type basety =
  | TyBool
  | TyInt
  | TyUnit

type ty =
  | TyParam of typaram
  | TyVar of tyvar
  | TyBase of basety
  | TyFun of ty * ty * ty * ty
  | TyDyn

let tyDynFun = TyFun (TyDyn, TyDyn, TyDyn, TyDyn)

(* Syntax *)

type id = string

type const =
  | ConstBool of bool
  | ConstInt of int
  | ConstUnit

type binop =
  | Plus
  | Minus
  | Mult
  | Div
  | Equal
  | Gt
  | Lt

(* Type Environment *)

module Environment = Map.Make (
  struct
    type t = id
    let compare (x : id) y = compare x y
  end
)

module GSR = struct
  type exp =
    | Var of id
    | Const of const
    | BinOp of binop * exp * exp
    | Fun of ty * id * ty * exp (* λ^12:3.4 *)
    | App of exp * exp
    | Shift of id * ty * exp (* S1:2.3 *)
    | Reset of exp * ty (* <1>^2 *)
    | If of exp * exp * exp
    | Consq of exp * exp

  type directive =
    | BoolDir of string * bool

  type program =
    | Exp of exp
    | Directive of directive

  let map f_ty f_exp = function
    | Var _ as e -> e
    | Const _ as e -> e
    | BinOp (op, e1, e2) -> BinOp (op, f_exp e1, f_exp e2)
    | Fun (g, x1, x1_t, e) -> Fun (f_ty g, x1, f_ty x1_t, f_exp e)
    | App (e1, e2) -> App (f_exp e1, f_exp e2)
    | Shift (k, k_t, e) -> Shift (k, f_ty k_t, f_exp e)
    | Reset (e, u) -> Reset (f_exp e, f_ty u)
    | If (e1, e2, e3) -> If (f_exp e1, f_exp e2, f_exp e3)
    | Consq (e1, e2) -> Consq (f_exp e1, f_exp e2)
end

module CSR = struct
  type exp =
    | Var of id
    | Const of const
    | BinOp of binop * exp * exp
    | Fun of ty * id * ty * exp
    | App of exp * exp
    | Shift of id * ty * exp
    | Reset of exp * ty
    | If of exp * exp * exp
    | Consq of exp * exp
    | Cast of exp * ty * ty

  let map f_ty f_exp = function
    | Var _ as e -> e
    | Const _ as e -> e
    | BinOp (op, e1, e2) -> BinOp (op, f_exp e1, f_exp e2)
    | Fun (g, x1, x1_t, e) -> Fun (f_ty g, x1, f_ty x1_t, f_exp e)
    | App (e1, e2) -> App (f_exp e1, f_exp e2)
    | Shift (k, k_t, e) -> Shift (k, f_ty k_t, f_exp e)
    | Reset (e, u) -> Reset (f_exp e, f_ty u)
    | If (e1, e2, e3) -> If (f_exp e1, f_exp e2, f_exp e3)
    | Consq (e1, e2) -> Consq (f_exp e1, f_exp e2)
    | Cast (e, u1, u2) -> Cast (f_exp e, f_ty u1, f_ty u2)

type tag = P of typaram | I | B | U | Ar

type value =
  | IntV of int
  | BoolV of bool
  | UnitV
  | FunV of (value -> cont -> value)
  | Tagged of tag * value
and cont = value -> value
end
