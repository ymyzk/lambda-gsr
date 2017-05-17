open Syntax
open Syntax.CSR

(* Hard errors *)
exception Eval_fatal_error of string
(* Soft errors *)
exception Blame of value * string

let rec eval exp env cont = match exp with
  | Var x ->
      if Environment.mem x env then
        cont @@ Environment.find x env
      else
        raise @@ Eval_fatal_error ("variable not found: " ^ x)
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
          | Equal, IntV x1, IntV x2 -> BoolV (x1 = x2)
          | Gt, IntV x1, IntV x2 -> BoolV (x1 > x2)
          | Lt, IntV x1, IntV x2 -> BoolV (x1 < x2)
          | _ -> raise @@ Eval_fatal_error "binop: unexpected type of the argument"
        end
  | Fun (_, x, _, f) ->
      cont @@ FunV (fun v -> fun c -> eval f (Environment.add x v env) c)
  | App (f1, f2) ->
      eval f1 env @@
        fun v1 -> begin
          match v1 with
            | FunV f ->
                eval f2 env @@
                fun v2 ->
                  f v2 cont
            | _ -> raise @@ Eval_fatal_error "app: application of non procedure value"
        end
(*
      eval f1 env @@
        fun v1 -> eval f2 env @@
        fun v2 -> begin
          match v1 with
            | FunV f -> f v2 cont
            | _ -> raise @@ Eval_fatal_error "app: application of non procedure value"
          end
*)
  | Shift (k, _, f) ->
      let env' = Environment.add k (FunV (fun v -> fun c -> c (cont v))) env in
      eval f env' @@ fun x -> x
  | Reset (f, _) ->
      cont @@ eval f env @@ fun x -> x
  | If (f1, f2, f3) ->
      eval f1 env @@
        fun v1 -> begin match v1 with
          | BoolV b -> if b then eval f2 env cont else eval f3 env cont
          | _ -> raise @@ Eval_fatal_error "if: non-bool condition"
        end
  | Consq (f1, f2) ->
      eval f1 env @@
        fun v1 -> begin match v1 with
          | UnitV -> eval f2 env cont
          | _ -> raise @@ Eval_fatal_error "consq: non-unit expression"
        end
  | Cast (f, u1, u2) ->
      eval f env @@ fun v -> cont @@ cast v u1 u2
and cast v u1 u2 = match u1, u2 with (* v: u1 => u2 *)
  (* Base *)
  | TyParam p1, TyParam p2 when p1 = p2 -> v
  | TyBase b1, TyBase b2 when b1 = b2 -> v
  (* Dyn *)
  | TyDyn, TyDyn -> v
  (* Wrap *)
  | TyFun (u11, u12, u13, u14), TyFun (u21, u22, u23, u24) ->
      FunV (fun b ->
        let a = cast b u21 u11 in
        fun k ->
          let k' = castk k (u23, u22) (u13, u12) in
          begin
            match v with
              | FunV f -> cast (f a k') u14 u24
              | _ -> raise @@ Eval_fatal_error "wrap"
          end)
  (* Ground *)
  | TyParam p1, TyDyn -> Tagged (P p1, (cast v u1 u1))
  | TyBase TyBool, TyDyn -> Tagged (B, (cast v u1 u1))
  | TyBase TyInt, TyDyn -> Tagged (I, (cast v u1 u1))
  | TyBase TyUnit, TyDyn -> Tagged (U, (cast v u1 u1))
  | TyFun _, TyDyn -> Tagged (Ar, (cast v u1 tyDynFun))
  (* Collapse / Conflict *)
  | TyDyn, TyParam p2 -> begin
      match v with
        | Tagged (P p1, v') when p1 = p2 -> cast v' u2 u2
        | Tagged (_, _) -> raise @@ Blame (v, "'a" ^ (string_of_int p2))
        | _ -> raise @@ Eval_fatal_error "untagged value 1"
      end
  | TyDyn, TyBase TyBool -> begin
      match v with
        | Tagged (B, v') -> cast v' (TyBase TyBool) (TyBase TyBool)
        | Tagged (_, _) -> raise @@ Blame (v, "bool")
        | _ -> raise @@ Eval_fatal_error "untagged value 2"
      end
  | TyDyn, TyBase TyInt -> begin
      match v with
        | Tagged (I, v') -> cast v' (TyBase TyInt) (TyBase TyInt)
        | Tagged (_, _) -> raise @@ Blame (v, "int")
        | _ -> raise @@ Eval_fatal_error "untagged value 3"
      end
  | TyDyn, TyBase TyUnit -> begin
      match v with
        | Tagged (U, v') -> cast v' (TyBase TyUnit) (TyBase TyUnit)
        | Tagged (_, _) -> raise @@ Blame (v, "unit")
        | _ -> raise @@ Eval_fatal_error "untagged value 4"
      end
  | TyDyn, TyFun _ -> begin
      match v with
        | Tagged (Ar, v') -> cast v' tyDynFun u2
        | Tagged (_, _) -> raise @@ Blame (v, "<fun>")
        | _ -> raise @@ Eval_fatal_error "untagged value 5"
      end
  | _ -> raise @@ Eval_fatal_error "cast is not implemented"
and castk k (u12, u13) (u22, u23) = fun v ->
  let v' = cast v u22 u12 in
  cast (k v') u13 u23
