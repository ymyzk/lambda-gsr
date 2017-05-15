open OUnit2

open Syntax
open Typing

let test_is_static_type =
  List.map
    (fun (l, t, e) -> l >:: fun _ -> assert_equal (is_static_type t) e)
    [
      "bool", TyBase TyBool, true;
      "int", TyBase TyInt, true;
      "int/unit -> bool/int", TyFun (TyBase TyInt, TyBase TyUnit, TyBase TyBool, TyBase TyInt), true;
      "int/unit -> ?/int", TyFun (TyBase TyInt, TyBase TyUnit, TyDyn, TyBase TyInt), false;
      "?", TyDyn, false;
    ]

let test_tyvars =
  List.map
    (fun (l, t, e) -> l >:: fun _ -> assert_equal (tyvars t) e)
    [
(*
      "int", TyInt, Variables.empty;
      "?", TyDyn, Variables.empty;
      "$0", TyVar 0, Variables.singleton 0;
      "int -> $0", TyFun (TyInt, TyVar 0), Variables.singleton 0;
      "$0 -> $0", TyFun (TyVar 0, TyVar 0), Variables.singleton 0;
      "$0 -> $1", TyFun (TyVar 0, TyVar 1), Variables.add 1 @@ Variables.singleton 0
*)
    ]

module CSR = struct
  open Syntax.GSR

  let test_subst_type =
    List.map
      (fun (l, x, t, u, e) -> l >:: fun _ -> assert_equal (subst_type x t u) e)
      [
        "int", 1, TyBase TyInt, TyBase TyInt, TyBase TyInt;
        "var1", 1, TyBase TyInt, TyVar 1, TyBase TyInt;
        "var2", 1, TyBase TyInt, TyVar 2, TyVar 2;
        "fun", 1, TyBase TyInt, TyFun (TyBase TyInt, TyVar 1, TyBase TyBool, TyVar 2), TyFun (TyBase TyInt, TyBase TyInt, TyBase TyBool, TyVar 2);
      ]

  let test_subst_exp =
    List.map
      (fun (l, x, t, u, e) -> l >:: fun _ -> assert_equal (subst_exp x t u) e)
      [
        "var", 1, TyBase TyInt, Var "x", Var "x";
        "fun", 2, TyBase TyInt, Fun (TyVar 1, "x", TyVar 2, Var "x"), Fun (TyVar 1, "x", TyBase TyInt, Var "x");
        "reset", 1, TyBase TyInt, Reset (Var "x", TyVar 1), Reset (Var "x", TyBase TyInt);
      ]
end

let suite = [
  "test_is_static_type">::: test_is_static_type;
  "test_tyvars">::: test_tyvars;
  "test_subst_type">::: CSR.test_subst_type;
  "test_subst_exp">::: CSR.test_subst_exp;
]
