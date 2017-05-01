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
