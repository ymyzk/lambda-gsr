open Constraints
open Syntax
open GSR (* FIXME *)

exception Type_error of string

(* Utilities *)

(* ty -> bool *)
let rec is_static_type = function
  | TyFun (t1, t2, t3, t4) -> (is_static_type t1) && (is_static_type t2) && (is_static_type t3) && (is_static_type t4)
  | TyDyn -> false
  | _ -> true

(* ty list -> bool *)
let is_static_types types = List.fold_left (&&) true @@ List.map is_static_type types

let is_base_type = function
  | TyInt | TyBool | TyUnit -> true
  | _ -> false

let is_tyvar = function
  | TyVar _ -> true
  | _ -> false

let is_typaram = function
  | TyParam _ -> true
  | _ -> false

let domf = function
  | TyFun (u, _, _, _) -> u
  | TyDyn -> TyDyn
  | _ -> raise @@ Type_error "failed to match"

let codc = function
  | TyFun (_, u, _, _) -> u
  | TyDyn -> TyDyn
  | _ -> raise @@ Type_error "failed to match"

let domc = function
  | TyFun (_, _, u, _) -> u
  | TyDyn -> TyDyn
  | _ -> raise @@ Type_error "failed to match"

let codf = function
  | TyFun (_, _, _, u) -> u
  | TyDyn -> TyDyn
  | _ -> raise @@ Type_error "failed to match"

let rec meet u1 u2 = match u1, u2 with
  | u1, u2 when u1 = u2 -> u1
  | u, TyDyn | TyDyn, u -> u
  | TyFun (u11, u12, u13, u14), TyFun (u21, u22, u23, u24) ->
      TyFun (meet u11 u21, meet u12 u22, meet u13 u23, meet u14 u24)
  | _ -> raise @@ Type_error "failed to meet"

(* Base type, type variables, or type parameters *)
let is_bvp_type t = is_base_type t || is_tyvar t || is_typaram t

(* Type Variables *)

module Variables = Set.Make(
  struct
    type t = tyvar
    let compare (x : tyvar) y = compare x y
  end
)

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    TyVar (v + 1)
  in body

let fresh_var =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    "!" ^ string_of_int @@ v + 1
  in body

let rec tyvars = function
  | TyVar x -> Variables.singleton x
  | TyFun (t1, t2, t3, t4) ->
      let vars = Variables.union (tyvars t1) (tyvars t2) in
      let vars = Variables.union vars (tyvars t3) in
      let vars = Variables.union vars (tyvars t4) in
      vars
  | _ -> Variables.empty

let rec tyvars_exp = function
  | Var _ -> Variables.empty
  | Const _ -> Variables.empty
  | BinOp (op, e1, e2) ->
      Variables.union (tyvars_exp e1) (tyvars_exp e2)
  | Fun (g, x1, x1_t, e) ->
      Variables.union (tyvars g)
      @@ Variables.union (tyvars x1_t)
      @@ tyvars_exp e
  | App (e1, e2) ->
      Variables.union (tyvars_exp e1) (tyvars_exp e2)
  | Shift (k, k_t, e) ->
      Variables.union (tyvars k_t) (tyvars_exp e)
  | Reset (e, u) ->
      Variables.union (tyvars_exp e) (tyvars u)
  | If (e1, e2, e3) ->
      Variables.union (tyvars_exp e1)
      @@ Variables.union (tyvars_exp e2)
      @@ tyvars_exp e3
  | Consq (e1, e2) ->
      Variables.union (tyvars_exp e1) (tyvars_exp e2)

(* Substitutions *)

type substitution = tyvar * ty
type substitutions = substitution list

(* [x:=t]u *)
let rec subst_type (x : tyvar) (t : ty) = function
  | TyFun (u1, u2, u3, u4) -> TyFun (subst_type x t u1, subst_type x t u2, subst_type x t u3, subst_type x t u4)
  | TyVar x' when x = x' -> t
  | _ as u -> u

(* [x:=t]e *)
let rec subst_exp x t e = GSR.map (subst_type x t) (subst_exp x t) e

(* [x:=t]c *)
let rec subst_constraint x t = function
  | ConstrEqual (u1, u2) -> ConstrEqual (subst_type x t u1, subst_type x t u2)
  | ConstrConsistent (u1, u2) -> ConstrConsistent (subst_type x t u1, subst_type x t u2)

(* [x:=t]C *)
let rec subst_constraints x t (c : constr list) =
  (* TODO: OK? *)
  List.map (subst_constraint x t) c

(* S(t) *)
let subst_type_substitutions (t : ty) (s : substitutions) =
  List.fold_left (fun u -> fun (x, t) -> subst_type x t u) t s

(* S(e) *)
let subst_exp_substitutions e (s : substitutions) =
  List.fold_left (fun e -> fun (x, t) -> subst_exp x t e) e s

let fresh_typaram =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    TyParam (v + 1)
  in body

module TyVarMap = Map.Make(
  struct
    type t = tyvar
    let compare (x : tyvar) y = compare x y
  end
)

let rec subst_tyvar m = function
  | TyVar x -> TyVarMap.find x m
  | TyFun (t1, t2, t3, t4) -> TyFun (subst_tyvar m t1, subst_tyvar m t2, subst_tyvar m t3, subst_tyvar m t4)
  | _ as t -> t

let rec subst_exp_tyvar m e = GSR.map (subst_tyvar m) (subst_exp_tyvar m) e

(* Create map from type parameters to type variables *)
let create_tyvar_typaram_map t =
  let f x m = if TyVarMap.mem x m then m else TyVarMap.add x (fresh_typaram ()) m in
  Variables.fold f (tyvars t) TyVarMap.empty

(* Create map from type parameters to type variables *)
let create_exp_tyvar_typaram_map e =
  let f x m = if TyVarMap.mem x m then m else TyVarMap.add x (fresh_typaram ()) m in
  Variables.fold f (tyvars_exp e) TyVarMap.empty

(* Replace type variables with type parameters *)
let subst_tyvars (t : ty) : ty =
  subst_tyvar (create_tyvar_typaram_map t) t

(* Replace type variables with type parameters *)
let subst_exp_tyvars e =
  subst_exp_tyvar (create_exp_tyvar_typaram_map e) e

(* Type Inference *)

(* domf = *)
let generate_constraints_domf_eq = function
| TyVar x ->
    let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
    x1, Constraints.singleton @@ ConstrEqual ((TyVar x), (TyFun (x1, x2, x3, x4)))
| TyFun (u1, u2, u3, u4) -> u1, Constraints.empty
| TyDyn -> TyDyn, Constraints.empty
| _ -> raise @@ Type_error "error"

(* domc = *)
let generate_constraints_domc_eq = function
| TyVar x ->
    let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
    x3, Constraints.singleton @@ ConstrEqual ((TyVar x), (TyFun (x1, x2, x3, x4)))
| TyFun (u1, u2, u3, u4) -> u3, Constraints.empty
| TyDyn -> TyDyn, Constraints.empty
| _ -> raise @@ Type_error "error"

(* codc = *)
let generate_constraints_codc_eq = function
| TyVar x ->
    let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
    x2, Constraints.singleton @@ ConstrEqual ((TyVar x), (TyFun (x1, x2, x3, x4)))
| TyFun (u1, u2, u3, u4) -> u2, Constraints.empty
| TyDyn -> TyDyn, Constraints.empty
| _ -> raise @@ Type_error "error"

(* codf = *)
let generate_constraints_codf_eq = function
| TyVar x ->
    let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
    x4, Constraints.singleton @@ ConstrEqual ((TyVar x), (TyFun (x1, x2, x3, x4)))
| TyFun (u1, u2, u3, u4) -> u4, Constraints.empty
| TyDyn -> TyDyn, Constraints.empty
| _ -> raise @@ Type_error "error"

(* domf ~ *)
let generate_constraints_domf_con u1 u2 = match u1 with
| TyVar x ->
    let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
    let c = Constraints.singleton @@ ConstrEqual ((TyVar x), (TyFun (x1, x2, x3, x4))) in
    Constraints.add (ConstrConsistent (x1, u2)) c
| TyFun (u11, u12, u13, u14) ->
    Constraints.singleton @@ ConstrConsistent (u11, u2)
| TyDyn -> Constraints.singleton @@ ConstrConsistent (u1, u2)
| _ -> raise @@ Type_error "error"

(* codf ~ *)
let generate_constraints_codf_con u1 u2 = match u1 with
| TyVar x ->
    let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
    let c = Constraints.singleton @@ ConstrEqual ((TyVar x), (TyFun (x1, x2, x3, x4))) in
    Constraints.add (ConstrConsistent (x4, u2)) c
| TyFun (u11, u12, u13, u14) ->
    Constraints.singleton @@ ConstrConsistent (u14, u2)
| TyDyn -> Constraints.singleton @@ ConstrConsistent (u1, u2)
| _ -> raise @@ Type_error "error"

let rec generate_constraints_join u1 u2 = match u1, u2 with
| TyInt, TyInt -> TyInt, Constraints.empty
| TyBool, TyBool -> TyBool, Constraints.empty
| TyUnit, TyUnit -> TyUnit, Constraints.empty
| _, TyDyn -> u1, Constraints.singleton @@ ConstrConsistent (u1, TyDyn)
| TyDyn, _ -> u2, Constraints.singleton @@ ConstrConsistent (TyDyn, u2)
| TyVar _, _ -> u1, Constraints.singleton @@ ConstrConsistent (u1, u2)
| _, TyVar _ -> u2, Constraints.singleton @@ ConstrConsistent (u1, u2)
| TyFun (u11, u12, u13, u14), TyFun (u21, u22, u23, u24) ->
    let u1, c1 = generate_constraints_join u11 u21 in
    let u2, c2 = generate_constraints_join u12 u22 in
    let u3, c3 = generate_constraints_join u13 u23 in
    let u4, c4 = generate_constraints_join u14 u24 in
    let c = Constraints.union c1 c2 in
    let c = Constraints.union c c3 in
    let c = Constraints.union c c4 in
    TyFun (u1, u2, u3, u4), c
| _ -> raise @@ Type_error "error: generate_constraints_join"

let generate_constraints env e b =
  let rec generate_constraints env e b =
    let t, a, c = match e with
      | Var x ->
          let u_a = b in (
          try
            let u = Environment.find x env in
            u, u_a, Constraints.empty
          with Not_found ->
            raise @@ Type_error (Printf.sprintf "variable '%s' not found in the environment" x)
          )
      | Const c ->
          let u_a = b in
          let u = begin
            match c with
            | ConstBool b -> TyBool
            | ConstInt i -> TyInt
            | ConstUnit -> TyUnit
            end
          in
          u, u_a, Constraints.empty
      | BinOp (op, e1, e2) ->
          let u_a0 = b in
          let u1, u_a1, c1 = generate_constraints env e1 u_a0 in
          let u2, u_a2, c2 = generate_constraints env e2 u_a1 in
          let c = Constraints.union c1
                  @@ Constraints.union c2
                  @@ Constraints.add (ConstrConsistent (u1, TyInt))
                  @@ Constraints.singleton
                  @@ ConstrConsistent (u2, TyInt) in
          TyInt, u_a2, c
      | Fun (u_g, x, u_1, e) ->
          let u_a = b in
          let u_2, u_b, c = generate_constraints (Environment.add x u_1 env) e u_g in
          TyFun (u_1, u_b, u_2, u_g), u_a, c
      | App (e1, e2) ->
          let u_d = b in
          let u_1, u_g, c1 = generate_constraints env e1 u_d in
          let u_2, u_b, c2 = generate_constraints env e2 u_g in
          let u, c3 = generate_constraints_domc_eq u_1 in
          let u_a, c4 = generate_constraints_codc_eq u_1 in
          let c5 = generate_constraints_codf_con u_1 u_b in
          let c6 = generate_constraints_domf_con u_1 u_2 in
          let c = Constraints.union c1
                  @@ Constraints.union c2
                  @@ Constraints.union c3
                  @@ Constraints.union c4
                  @@ Constraints.union c5 c6 in
          u, u_a, c
      | Shift (k, u_s, e) ->
          let u_b = b in
          let u_d, u_d', c1 = generate_constraints (Environment.add k u_s env) e u_b in
          let u_a, c2 = generate_constraints_domc_eq u_s in
          let u, c3 = generate_constraints_domf_eq u_s in
          let u_g1, c4 = generate_constraints_codc_eq u_s in
          let u_g2, c5 = generate_constraints_codf_eq u_s in
          let _, c6 = generate_constraints_join u_g1 u_g2 in
          let c = Constraints.union c1
                  @@ Constraints.union c2
                  @@ Constraints.union c3
                  @@ Constraints.union c4
                  @@ Constraints.union c5
                  @@ Constraints.union c6
                  @@ Constraints.singleton
                  @@ ConstrConsistent (u_d, u_d') in
          u, u_a, c
      | Reset (e, u) ->
          let u_a = b in
          let u_b, u_b', c = generate_constraints env e u in
          let c = Constraints.add (ConstrConsistent (u_b, u_b')) c in
          u, u_a, c
      | If (e1, e2, e3) ->
          let u_b = b in
          let u_1, u_d, c1 = generate_constraints env e1 u_b in
          let u_2, u_a2, c2 = generate_constraints env e2 u_d in
          let u_3, u_a3, c3 = generate_constraints env e3 u_d in
          let u_a, c4 = generate_constraints_join u_a2 u_a3 in
          let u, c5 = generate_constraints_join u_2 u_3 in
          let c = Constraints.union c1
                  @@ Constraints.union c2
                  @@ Constraints.union c3
                  @@ Constraints.union c4
                  @@ Constraints.union c5
                  @@ Constraints.singleton
                  @@ ConstrConsistent (u_1, TyBool) in
          u, u_a, c
      | Consq (e1, e2) ->
          let u_g = b in
          let u_1, u_b, c1 = generate_constraints env e1 u_g in
          let u_2, u_a, c2 = generate_constraints env e2 u_b in
          let c = Constraints.union c1
                  @@ Constraints.union c2
                  @@ Constraints.singleton
                  @@ ConstrConsistent (u_1, TyUnit) in
          u_2, u_a, c
    in
    (* logging *)
    (*
    print_endline @@ Printf.sprintf "%s; |- %s: %s; %s" (string_of_type a) (string_of_exp e) (string_of_type t) (string_of_type b);
    print_endline @@ string_of_constraints c;
    *)
    t, a, c
  in
  generate_constraints env e b

let unify constraints : substitutions =
  let rec unify c =
    match c with
    | [] -> []
    | constr :: c -> begin
      match constr with
      | ConstrConsistent (u1, u2) when u1 = u2 && is_bvp_type u1 ->
          unify c
      | ConstrConsistent (TyDyn, _)
      | ConstrConsistent (_, TyDyn) ->
          unify c
      | ConstrConsistent (TyFun (u11, u12, u13, u14), TyFun (u21, u22, u23, u24)) ->
          unify @@ ConstrConsistent (u11, u21) :: ConstrConsistent (u12, u22) :: ConstrConsistent (u13, u23) :: ConstrConsistent (u14, u24) :: c
      | ConstrConsistent (u, TyVar x) when not @@ is_tyvar u ->
          unify @@ ConstrConsistent (TyVar x, u) :: c
      | ConstrConsistent (TyVar x, u) when is_bvp_type u ->
          unify @@ ConstrEqual (TyVar x, u) :: c
      | ConstrConsistent (TyVar x, TyFun (u1, u2, u3, u4)) when not @@ Variables.mem x (tyvars (TyFun (u1, u2, u3, u4))) ->
          let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
          unify @@ ConstrEqual (TyVar x, TyFun (x1, x2, x3, x4)) :: ConstrConsistent (x1, u1) :: ConstrConsistent (x2, u2) :: ConstrConsistent (x3, u3) :: ConstrConsistent (x4, u4) :: c
      | ConstrEqual (t1, t2) when t1 = t2 && is_static_type t1 && is_bvp_type t1 ->
          unify c
      | ConstrEqual (TyFun (t11, t12, t13, t14), TyFun (t21, t22, t23, t24)) when is_static_types [t11; t12; t13; t14; t21; t22; t23; t24] ->
          unify @@ ConstrEqual (t11, t21) :: ConstrEqual (t12, t22) :: ConstrEqual (t13, t23) :: ConstrEqual (t14, t24) :: c
      | ConstrEqual (t, TyVar x) when is_static_type t && not (is_tyvar t) ->
          unify @@ ConstrEqual (TyVar x, t) :: c
      | ConstrEqual (TyVar x, t) when not @@ Variables.mem x (tyvars t) ->
          let s = unify @@ subst_constraints x t c in
          (x, t) :: s
      | _ ->
          raise @@ Type_error ("cannot unify: " ^ (Pp.string_of_constr constr))
    end
  in
  unify @@ Constraints.map (fun x -> x) constraints

let infer env e b =
  let u, a, c = generate_constraints env e b in
  let s = unify c in
  let e = subst_exp_substitutions e s in
  let u = subst_type_substitutions u s in
  let a = subst_type_substitutions a s in
  let b = subst_type_substitutions b s in
  subst_exp_tyvars e, subst_tyvars u, subst_tyvars a, subst_tyvars b

module GSR = struct
  open Syntax
  open Syntax.GSR

  let rec translate env e u_b = match e with
    | Var x -> begin
        try
          let u = Environment.find x env in
          CSR.Var x, u, u_b
        with Not_found ->
          raise @@ Type_error (Printf.sprintf "variable '%s' not found in the environment" x)
      end
    | Const c ->
        let u = begin
          match c with
          | ConstBool b -> TyBool
          | ConstInt i -> TyInt
          | ConstUnit -> TyUnit
          end
        in
        CSR.Const c, u, u_b
    | BinOp (op, e1, e2) ->
        let f1, u1, u1_a = translate env e1 u_b in
        let f2, u2, u2_a = translate env e2 u1_a in
        (* TODO CONSITENCY CHECK *)
        CSR.BinOp (op, f1, f2), TyInt, u2_a
    | Fun (u_g, x, u_1, e) ->
        let f, u_2, u_b = translate (Environment.add x u_1 env) e u_g in
        CSR.Fun (x, u_1, f), TyFun (u_1, u_b, u_2, u_g), u_b
    | App (e1, e2) ->
        let f1, u1, u_g = translate env e1 u_b in
        let f2, u2, u_b = translate env e2 u_g in
        (* TODO CONSITENCY CHECK *)
        CSR.App (CSR.Cast (f1, u1, TyFun (domf u1, codc u1, domc u1, u_b)),
                 CSR.Cast(f2, u2, domf u1)),
        domc u1,
        codc u1
    | Shift (k, u_s, e) ->
        let f, u_d, u_d' = translate (Environment.add k u_s env) e u_b in
        let u_g = meet (codc u_s) (codf u_s) in
        (* TODO CONSITENCY CHECK *)
        let k' = fresh_var () in
        CSR.Shift (k',
                   CSR.App (CSR.Fun (k, u_s, CSR.Cast(f, u_d, u_d')),
                            CSR.Cast (CSR.Var k', TyFun (domf u_s, u_g, domc u_s, u_g), u_s))),
        domf u_s,
        domc u_s
    | Reset (e, u) ->
        let u_a = u_b in
        let f, u_b, u_b' = translate env e u in
        (* TODO CONSITENCY CHECK *)
        CSR.Reset f, u, u_a
end
