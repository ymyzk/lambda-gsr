open Constraints
open Syntax

exception Type_error of string

(* Utilities *)

let is_tyvar = function TyVar _ -> true | _ -> false

(* ty -> bool *)
let rec is_static_type = function
  | TyFun (t1, t2, t3, t4) -> (is_static_type t1) && (is_static_type t2) && (is_static_type t3) && (is_static_type t4)
  | TyDyn -> false
  | _ -> true

(* ty list -> bool *)
let is_static_types types = List.fold_left (&&) true @@ List.map is_static_type types

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

let rec is_consistent u1 u2 = match u1, u2 with
  | TyParam p1, TyParam p2 when p1 = p2 -> true
  | TyBase b1, TyBase b2 when b1 = b2 -> true
  | _, TyDyn
  | TyDyn, _ -> true
  | TyFun (u11, u12, u13, u14), TyFun (u21, u22, u23, u24) ->
      is_consistent u11 u21 &&
      is_consistent u12 u22 &&
      is_consistent u13 u23 &&
      is_consistent u14 u24
  | _ -> false

let rec meet u1 u2 = match u1, u2 with
  | u1, u2 when u1 = u2 -> u1
  | u, TyDyn | TyDyn, u -> u
  | TyFun (u11, u12, u13, u14), TyFun (u21, u22, u23, u24) ->
      TyFun (meet u11 u21, meet u12 u22, meet u13 u23, meet u14 u24)
  | _ -> raise @@ Type_error "failed to meet"

(* Base type, type variables, or type parameters *)
let is_bvp_type = function
  | TyBase _ -> true
  | TyVar _ -> true
  | TyParam _ -> true
  | _ -> false

let type_of_const c = TyBase (match c with
  | ConstBool _ -> TyBool
  | ConstInt _ -> TyInt
  | ConstUnit -> TyUnit
)

let type_of_binop = function
  | Plus | Minus | Mult | Div -> TyBase TyInt, TyBase TyInt, TyBase TyInt
  | Equal | Gt | Lt -> TyBase TyInt, TyBase TyInt, TyBase TyBool

let fresh_var =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    "!" ^ string_of_int @@ v + 1
  in body

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
let subst_constraint x t = function
  | CEqual (u1, u2) -> CEqual (subst_type x t u1, subst_type x t u2)
  | CConsistent (u1, u2) -> CConsistent (subst_type x t u1, subst_type x t u2)

(* [x:=t]C *)
let subst_constraints x t (c : constr list) =
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

module GSR = struct
  open Syntax.GSR

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
    | BinOp (_, e1, e2) ->
        Variables.union (tyvars_exp e1) (tyvars_exp e2)
    | Fun (g, _, x1_t, e) ->
        Variables.union (tyvars g)
        @@ Variables.union (tyvars x1_t)
        @@ tyvars_exp e
    | App (e1, e2) ->
        Variables.union (tyvars_exp e1) (tyvars_exp e2)
    | Shift (_, k_t, e) ->
        Variables.union (tyvars k_t) (tyvars_exp e)
    | Reset (e, u) ->
        Variables.union (tyvars_exp e) (tyvars u)
    | If (e1, e2, e3) ->
        Variables.union (tyvars_exp e1)
        @@ Variables.union (tyvars_exp e2)
        @@ tyvars_exp e3
    | Consq (e1, e2) ->
        Variables.union (tyvars_exp e1) (tyvars_exp e2)

  (* Type Variables -> Type Parameters *)

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

  (* Replace type variables with type parameters *)
  let subst_tyvars tvm t =
    let f x m = if TyVarMap.mem x m then m else TyVarMap.add x (fresh_typaram ()) m in
    let tvm = Variables.fold f (tyvars t) tvm in
    tvm, subst_tyvar tvm t

  (* Replace type variables with type parameters *)
  let subst_exp_tyvars tvm e =
    let f x m = if TyVarMap.mem x m then m else TyVarMap.add x (fresh_typaram ()) m in
    let tvm = Variables.fold f (tyvars_exp e) tvm in
    tvm, subst_exp_tyvar tvm e

  (* Type Inference *)

  (* domf = *)
  let generate_constraints_domf_eq = function
    | TyVar x ->
        let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
        x1, Constraints.singleton @@ CEqual ((TyVar x), (TyFun (x1, x2, x3, x4)))
    | TyFun (u1, _, _, _) -> u1, Constraints.empty
    | TyDyn -> TyDyn, Constraints.empty
    | _ -> raise @@ Type_error "error"

  (* domc = *)
  let generate_constraints_domc_eq = function
    | TyVar x ->
        let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
        x3, Constraints.singleton @@ CEqual ((TyVar x), (TyFun (x1, x2, x3, x4)))
    | TyFun (_, _, u3, _) -> u3, Constraints.empty
    | TyDyn -> TyDyn, Constraints.empty
    | _ -> raise @@ Type_error "error"

  (* codc = *)
  let generate_constraints_codc_eq = function
    | TyVar x ->
        let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
        x2, Constraints.singleton @@ CEqual ((TyVar x), (TyFun (x1, x2, x3, x4)))
    | TyFun (_, u2, _, _) -> u2, Constraints.empty
    | TyDyn -> TyDyn, Constraints.empty
    | _ -> raise @@ Type_error "error"

  (* codf = *)
  let generate_constraints_codf_eq = function
    | TyVar x ->
        let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
        x4, Constraints.singleton @@ CEqual ((TyVar x), (TyFun (x1, x2, x3, x4)))
    | TyFun (_, _, _, u4) -> u4, Constraints.empty
    | TyDyn -> TyDyn, Constraints.empty
    | _ -> raise @@ Type_error "error"

  (* domf ~ *)
  let generate_constraints_domf_con u1 u2 = match u1 with
    | TyVar x ->
        let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
        let c = Constraints.singleton @@ CEqual ((TyVar x), (TyFun (x1, x2, x3, x4))) in
        Constraints.add (CConsistent (x1, u2)) c
    | TyFun (u11, _, _, _) ->
        Constraints.singleton @@ CConsistent (u11, u2)
    | TyDyn -> Constraints.singleton @@ CConsistent (u1, u2)
    | _ -> raise @@ Type_error "error"

  (* codf ~ *)
  let generate_constraints_codf_con u1 u2 = match u1 with
    | TyVar x ->
        let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
        let c = Constraints.singleton @@ CEqual ((TyVar x), (TyFun (x1, x2, x3, x4))) in
        Constraints.add (CConsistent (x4, u2)) c
    | TyFun (_, _, _, u14) ->
        Constraints.singleton @@ CConsistent (u14, u2)
    | TyDyn -> Constraints.singleton @@ CConsistent (u1, u2)
    | _ -> raise @@ Type_error "error"

  let rec generate_constraints_meet u1 u2 = match u1, u2 with
    | TyBase b1, TyBase b2 when b1 = b2 -> TyBase b1, Constraints.empty
    | _, TyDyn -> u1, Constraints.singleton @@ CConsistent (u1, TyDyn)
    | TyDyn, _ -> u2, Constraints.singleton @@ CConsistent (TyDyn, u2)
    | TyVar _, _ -> u1, Constraints.singleton @@ CConsistent (u1, u2)
    | _, TyVar _ -> u2, Constraints.singleton @@ CConsistent (u1, u2)
    | TyFun (u11, u12, u13, u14), TyFun (u21, u22, u23, u24) ->
        let u1, c1 = generate_constraints_meet u11 u21 in
        let u2, c2 = generate_constraints_meet u12 u22 in
        let u3, c3 = generate_constraints_meet u13 u23 in
        let u4, c4 = generate_constraints_meet u14 u24 in
        let c = Constraints.union c1 c2 in
        let c = Constraints.union c c3 in
        let c = Constraints.union c c4 in
        TyFun (u1, u2, u3, u4), c
    | _ -> raise @@ Type_error "error: generate_constraints_meet"

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
            let u = type_of_const c in
            u, u_a, Constraints.empty
        | BinOp (op, e1, e2) ->
            let ui1, ui2, ui = type_of_binop op in
            let u_a0 = b in
            let u1, u_a1, c1 = generate_constraints env e1 u_a0 in
            let u2, u_a2, c2 = generate_constraints env e2 u_a1 in
            let c = Constraints.union c1
                    @@ Constraints.union c2
                    @@ Constraints.add (CConsistent (u1, ui1))
                    @@ Constraints.singleton
                    @@ CConsistent (u2, ui2) in
            ui, u_a2, c
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
            let _, c6 = generate_constraints_meet u_g1 u_g2 in
            let c = Constraints.union c1
                    @@ Constraints.union c2
                    @@ Constraints.union c3
                    @@ Constraints.union c4
                    @@ Constraints.union c5
                    @@ Constraints.union c6
                    @@ Constraints.singleton
                    @@ CConsistent (u_d, u_d') in
            u, u_a, c
        | Reset (e, u) ->
            let u_a = b in
            let u_b, u_b', c = generate_constraints env e u in
            let c = Constraints.add (CConsistent (u_b, u_b')) c in
            u, u_a, c
        | If (e1, e2, e3) ->
            let u_b = b in
            let u_1, u_d, c1 = generate_constraints env e1 u_b in
            let u_2, u_a2, c2 = generate_constraints env e2 u_d in
            let u_3, u_a3, c3 = generate_constraints env e3 u_d in
            let u_a, c4 = generate_constraints_meet u_a2 u_a3 in
            let u, c5 = generate_constraints_meet u_2 u_3 in
            let c = Constraints.union c1
                    @@ Constraints.union c2
                    @@ Constraints.union c3
                    @@ Constraints.union c4
                    @@ Constraints.union c5
                    @@ Constraints.singleton
                    @@ CConsistent (u_1, TyBase TyBool) in
            u, u_a, c
        | Consq (e1, e2) ->
            let u_g = b in
            let u_1, u_b, c1 = generate_constraints env e1 u_g in
            let u_2, u_a, c2 = generate_constraints env e2 u_b in
            let c = Constraints.union c1
                    @@ Constraints.union c2
                    @@ Constraints.singleton
                    @@ CConsistent (u_1, TyBase TyUnit) in
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
      | CConsistent (u1, u2) when u1 = u2 && is_bvp_type u1 ->
          unify c
      | CConsistent (TyDyn, _)
      | CConsistent (_, TyDyn) ->
          unify c
      | CConsistent (TyFun (u11, u12, u13, u14), TyFun (u21, u22, u23, u24)) ->
          unify @@ CConsistent (u11, u21) :: CConsistent (u12, u22) :: CConsistent (u13, u23) :: CConsistent (u14, u24) :: c
      | CConsistent (u, TyVar x) when not @@ is_tyvar u ->
          unify @@ CConsistent (TyVar x, u) :: c
      | CConsistent (TyVar x, u) when is_bvp_type u ->
          unify @@ CEqual (TyVar x, u) :: c
      | CConsistent (TyVar x, TyFun (u1, u2, u3, u4)) when not @@ Variables.mem x (tyvars (TyFun (u1, u2, u3, u4))) ->
          let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
          unify @@ CEqual (TyVar x, TyFun (x1, x2, x3, x4)) :: CConsistent (x1, u1) :: CConsistent (x2, u2) :: CConsistent (x3, u3) :: CConsistent (x4, u4) :: c
      | CEqual (t1, t2) when t1 = t2 && is_static_type t1 && is_bvp_type t1 ->
          unify c
      | CEqual (TyFun (t11, t12, t13, t14), TyFun (t21, t22, t23, t24)) when is_static_types [t11; t12; t13; t14; t21; t22; t23; t24] ->
          unify @@ CEqual (t11, t21) :: CEqual (t12, t22) :: CEqual (t13, t23) :: CEqual (t14, t24) :: c
      | CEqual (t, TyVar x) when is_static_type t && not (is_tyvar t) ->
          unify @@ CEqual (TyVar x, t) :: c
      | CEqual (TyVar x, t) when not @@ Variables.mem x (tyvars t) ->
          let s = unify @@ subst_constraints x t c in
          (x, t) :: s
      | _ ->
          raise @@ Type_error ("cannot unify: " ^ (Pp.string_of_constr constr))
    end
  in
  unify @@ Constraints.map (fun x -> x) constraints

let infer ?(debug=false) env e b =
  let u, a, c = generate_constraints env e b in
  if debug then
    prerr_endline @@ "Constraints: " ^ Pp.string_of_constraints c;
  let s = unify c in
  if debug then
    prerr_endline @@ "Substitutions: " ^ Pp.string_of_substitutions s;
  let e = subst_exp_substitutions e s in
  let u = subst_type_substitutions u s in
  let a = subst_type_substitutions a s in
  let b = subst_type_substitutions b s in
  if debug then begin
    prerr_endline "After Substitution:";
    prerr_endline @@ " e: " ^ Pp.GSR.string_of_exp e;
    prerr_endline @@ " U: " ^ Pp.string_of_type u;
    prerr_endline @@ " Uα: " ^ Pp.string_of_type a;
    prerr_endline @@ " Uβ: " ^ Pp.string_of_type b
  end;
  let tvm = TyVarMap.empty in
  let tvm, e = subst_exp_tyvars tvm e in
  let tvm, u = subst_tyvars tvm u in
  let tvm, a = subst_tyvars tvm a in
  let _, b = subst_tyvars tvm b in
  e, u, a, b

  let rec translate env e u_b = match e with
    | Var x -> begin
        try
          let u = Environment.find x env in
          CSR.Var x, u, u_b
        with Not_found ->
          raise @@ Type_error (Printf.sprintf "variable '%s' not found in the environment" x)
      end
    | Const c ->
        let u = type_of_const c in
        CSR.Const c, u, u_b
    | BinOp (op, e1, e2) ->
        let ui1, ui2, ui = type_of_binop op in
        let f1, u1, u1_a = translate env e1 u_b in
        let f2, u2, u2_a = translate env e2 u1_a in
        begin match is_consistent u1 ui1, is_consistent u2 ui2 with
          | true, true -> CSR.BinOp (op, CSR.Cast (f1, u1, ui1), CSR.Cast (f2, u2, ui2)), ui, u2_a
          | false, _ -> raise @@ Type_error (Printf.sprintf "binop: the first argument has type %s but is expected to have type %s" (Pp.string_of_type u1) (Pp.string_of_type ui1))
          | _, false -> raise @@ Type_error (Printf.sprintf "binop: the second argument has type %s but is expected to have type %s" (Pp.string_of_type u2) (Pp.string_of_type ui2))
        end
    | Fun (u_g, x, u_1, e) ->
        let u_a = u_b in
        let f, u_2, u_b = translate (Environment.add x u_1 env) e u_g in
        CSR.Fun (u_g, x, u_1, f), TyFun (u_1, u_b, u_2, u_g), u_a
    | App (e1, e2) ->
        let f1, u1, u_g = translate env e1 u_b in
        let f2, u2, u_b = translate env e2 u_g in
        begin match is_consistent (codf u1) u_b, is_consistent (domf u1) u2 with
          | true, true ->
              CSR.App (CSR.Cast (f1, u1, TyFun (domf u1, codc u1, domc u1, u_b)),
                       CSR.Cast(f2, u2, domf u1)),
              domc u1,
              codc u1
          | _ -> raise @@ Type_error "app: not consistent"
        end
    | Shift (k, u_s, e) ->
        let f, u_d, u_d' = translate (Environment.add k u_s env) e u_b in
        let u_g = meet (codc u_s) (codf u_s) in
        let k' = fresh_var () in
        begin match is_consistent u_d u_d', is_consistent (codc u_s) (codf u_s) with
          | true, true ->
              CSR.Shift (
                k', TyFun (domf u_s, u_g, domc u_s, u_g),
                CSR.App (CSR.Fun (u_b, k, u_s, CSR.Cast(f, u_d, u_d')),
                         CSR.Cast (CSR.Var k', TyFun (domf u_s, u_g, domc u_s, u_g), u_s))),
              domf u_s,
              domc u_s
          | _ -> raise @@ Type_error "shift: not consistent"
        end
    | Reset (e, u) ->
        let u_a = u_b in
        let f, u_b, u_b' = translate env e u in
        if is_consistent u_b u_b' then
          CSR.Reset (CSR.Cast (f, u_b, u_b'), u), u, u_a
        else
          raise @@ Type_error "reset: not consistent"
    | If (e1, e2, e3) ->
        let f1, u1, u_d = translate env e1 u_b in
        let f2, u2, u_a2 = translate env e2 u_d in
        let f3, u3, u_a3 = translate env e3 u_d in
        let u_a = meet u_a2 u_a3 in
        let u = meet u2 u3 in
        let k2 = fresh_var () in
        let k3 = fresh_var () in
        if is_consistent u1 @@ TyBase TyBool then
          CSR.If (
            CSR.Cast (f1, u1, (TyBase TyBool)),
            CSR.Shift (k2, TyFun (u, u_a, u_a, u_a), CSR.App (
              CSR.Cast (CSR.Var k2, TyFun (u, u_a, u_a, u_a),
                                    TyFun (u, u_a, u_a, u_a2)),
              CSR.Cast(f2, u2, u))),
            CSR.Shift (k3, TyFun (u, u_a, u_a, u_a), CSR.App (
              CSR.Cast (CSR.Var k3, TyFun (u, u_a, u_a, u_a),
                                    TyFun (u, u_a, u_a, u_a3)),
              CSR.Cast(f3, u3, u)))
          ), u, u_a
        else
          raise @@ Type_error "if: not consistent"
    | Consq (e1, e2) ->
        let u_g = u_b in
        let f1, u1, u_b = translate env e1 u_g in
        let f2, u2, u_a = translate env e2 u_b in
        if is_consistent u1 @@ TyBase TyUnit then
          CSR.Consq ((CSR.Cast (f1, u1, TyBase TyUnit)), f2), u2, u_a
        else
          raise @@ Type_error "consq: not consistent"
end

module CSR = struct
  open Syntax.CSR

  let rec type_of_exp env f ub = match f with
    | Var x -> begin
        try
          let u = Environment.find x env in
          u, ub
        with Not_found ->
          raise @@ Type_error (Printf.sprintf "variable '%s' not found in the environment" x)
      end
    | Const c ->
        let u = type_of_const c in
        u, ub
    | BinOp (op, f1, f2) ->
        let ui1, ui2, ui = type_of_binop op in
        let u1, ua1 = type_of_exp env f1 ub in
        let u2, ua2 = type_of_exp env f2 ua1 in
        begin match u1 = ui1, u2 = ui2 with
          | true, true -> ui, ua2
          | false, _ -> raise @@ Type_error (Printf.sprintf "binop: the first argument has type %s but is expected to have type %s" (Pp.string_of_type u1) (Pp.string_of_type ui1))
          | _, false -> raise @@ Type_error (Printf.sprintf "binop: the second argument has type %s but is expected to have type %s" (Pp.string_of_type u2) (Pp.string_of_type ui2))
        end
    | Fun (ug, x, u1, f) ->
        let ua = ub in
        let u2, ub = type_of_exp (Environment.add x u1 env) f ug in
        TyFun (u1, ub, u2, ug), ua
    | App (f1, f2) ->
        let ud = ub in
        let u1, ug = type_of_exp env f1 ud in
        let u2, ub = type_of_exp env f2 ug in
        begin match u1, (codf u1) = ub, (domf u1) = u2 with
          | TyFun _, true, true -> domc u1, codc u1
          | _ -> raise @@ Type_error "app"
        end
    | Shift (k, us, f) ->
        let ud, ud' = type_of_exp (Environment.add k us env) f ub in
        begin match us, (codc us) = (codf us), ud = ud' with
          | TyFun _, true, true -> domf us, domc us
          | _ -> raise @@ Type_error "shift"
        end
    | Reset (f, u) ->
        let ua = ub in
        let ub, ub' = type_of_exp env f u in
        if ub = ub' then
          u, ua
        else
          raise @@ Type_error "reset"
    | If (f1, f2, f3) ->
        let u1, ud = type_of_exp env f1 ub in
        let u2, ua2 = type_of_exp env f2 ud in
        let u3, ua3 = type_of_exp env f3 ud in
        begin match u1 = TyBase TyBool, u2 = u3, ua2 = ua3 with
          | true, true, true -> u2, ua2
          | _ -> raise @@ Type_error "if"
        end
    | Consq (f1, f2) ->
        let ug = ub in
        let u1, ub = type_of_exp env f1 ug in
        let u2, ua = type_of_exp env f2 ub in
        if u1 = TyBase TyUnit then
          u2, ua
        else
          raise @@ Type_error "consq"
    | Cast (f, u1, u2) ->
        let u1', ua = type_of_exp env f ub in
        begin match u1 = u1', is_consistent u1 u2 with
          | true, true -> u2, ua
          | _ -> raise @@ Type_error "cast"
        end
end
