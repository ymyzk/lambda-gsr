(* Types *)

type typaram = int
type tyvar = int

type ty =
  | TyParam of typaram
  | TyVar of tyvar
  | TyBool
  | TyInt
  | TyUnit
  | TyFun of ty * ty * ty * ty
  | TyDyn

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
    | Fun of id * ty * exp
    | App of exp * exp
    | Shift of id * exp
    | Reset of exp
    | If of exp * exp * exp
    | Consq of exp * exp
    | Cast of exp * ty * ty
end
