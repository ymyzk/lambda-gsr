open Syntax
open Syntax.CSR

exception Eval_error of string

type tag = P of typaram | I | B | U | Ar

type value =
  | IntV of int
  | BoolV of bool
  | UnitV
  | FunV of (value -> cont -> value)
  | Tagged of tag * value
and cont = value -> value

let rec eval exp env cont = match exp with
  | Var x ->
      if Environment.mem x env then
        cont @@ Environment.find x env
      else
        raise @@ Eval_error "variable not found"
  | Const c -> begin
      match c with
        | ConstBool b -> cont @@ BoolV b
        | ConstInt i -> cont @@ IntV i
        | ConstUnit -> cont UnitV
      end
  | BinOp (op, f1, f2) ->
      eval f1 env @@
        fun x1 -> eval f2 env @@
        fun x2 -> cont begin
          match op, x1, x2 with
          | Plus, IntV x1, IntV x2 -> IntV (x1 + x2)
          | Minus, IntV x1, IntV x2 -> IntV (x1 - x2)
          | Mult, IntV x1, IntV x2 -> IntV (x1 - x2)
          | Div, IntV x1, IntV x2 -> IntV (x1 / x2)
          | _ -> raise @@ Eval_error "binop"
        end
  | Fun (x, _, f) ->
      cont @@ FunV (fun v -> fun c -> eval f (Environment.add x v env) c)
  | App (f1, f2) ->
      eval f1 env @@
        fun v1 -> eval f2 env @@
        fun v2 -> begin
          match v1 with
            | FunV f -> f v2 cont
            | _ -> raise @@ Eval_error "application"
          end
  | Shift (k, f) ->
      let env' = Environment.add k (FunV (fun v -> fun c -> c (cont v))) env in
      eval f env' @@ fun x -> x
  | Reset f ->
      cont @@ eval f env @@ fun x -> x
  | Cast (f, u1, u2) ->
      eval f env @@ fun v -> cont @@ cast v u1 u2
and cast v u1 u2 = match u1, u2 with (* v: u1 => u2 *)
  (* Base *)
  | TyParam p1, TyParam p2 when p1 = p2 -> v
  | TyBool, TyBool | TyInt, TyInt | TyUnit, TyUnit -> v
  (* Dyn *)
  | TyDyn, TyDyn -> v
  (* Wrap *)
  | TyFun (u11, u12, u13, u14), TyFun (u21, u22, u23, u24) ->
      FunV (fun b ->
        let a = cast b u21 u11 in
        fun k ->
          let k' = castk k (u22, u23) (u12, u13) in
          begin
            match v with
              | FunV f -> cast (f a k') u14 u24
              | _ -> raise @@ Eval_error "fun cast"
          end)
  (* Ground *)
  | TyParam p1, TyDyn -> Tagged (P p1, (cast v u1 u1))
  | TyBool, TyDyn -> Tagged (B, (cast v u1 u1))
  | TyInt, TyDyn -> Tagged (I, (cast v u1 u1))
  | TyUnit, TyDyn -> Tagged (U, (cast v u1 u1))
  | TyFun _, TyDyn -> Tagged (Ar, (cast v u1 @@ TyFun (TyDyn, TyDyn, TyDyn, TyDyn)))
  (* Collapse / Conflict *)
  | TyDyn, TyParam p2 -> begin
      match v with
        | Tagged (P p1, v') when p1 = p2 -> cast v' u2 u2
        | Tagged (_, _) -> raise @@ Eval_error "blame"
        | _ -> raise @@ Eval_error "untagged value"
      end
  | TyDyn, TyBool -> begin
      match v with
        | Tagged (B, v') -> cast v' TyBool TyBool
        | Tagged (_, _) -> raise @@ Eval_error "blame"
        | _ -> raise @@ Eval_error "untagged value"
      end
  | TyDyn, TyInt -> begin
      match v with
        | Tagged (I, v') -> cast v' TyInt TyInt
        | Tagged (_, _) -> raise @@ Eval_error "blame"
        | _ -> raise @@ Eval_error "untagged value"
      end
  | TyDyn, TyUnit -> begin
      match v with
        | Tagged (U, v') -> cast v' TyUnit TyUnit
        | Tagged (_, _) -> raise @@ Eval_error "blame"
        | _ -> raise @@ Eval_error "untagged value"
      end
  | TyDyn, TyFun _ -> begin
      match v with
        | Tagged (Ar, v') -> cast v' (TyFun (TyDyn, TyDyn, TyDyn, TyDyn)) u2
        | Tagged (_, _) -> raise @@ Eval_error "blame"
        | _ -> raise @@ Eval_error "untagged value"
      end
  | _ -> raise @@ Eval_error "cast is not implemented"
and castk k (u12, u13) (u22, u23) = fun v ->
  let v' = cast v u12 u22 in
  cast (k v') u23 u13
