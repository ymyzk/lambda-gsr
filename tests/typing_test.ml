open OUnit2

open Syntax
open Typing

let test_is_static_type =
  List.map
    (fun (l, t, e) -> l >:: fun _ -> assert_equal (is_static_type t) e)
    [
      "bool", TyBool, true;
      "int", TyInt, true;
      "int/unit -> bool/int", TyFun (TyInt, TyUnit, TyBool, TyInt), true;
      "int/unit -> ?/int", TyFun (TyInt, TyUnit, TyDyn, TyInt), false;
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
        "int", 1, TyInt, TyInt, TyInt;
        "var1", 1, TyInt, TyVar 1, TyInt;
        "var2", 1, TyInt, TyVar 2, TyVar 2;
        "fun", 1, TyInt, TyFun (TyInt, TyVar 1, TyBool, TyVar 2), TyFun (TyInt, TyInt, TyBool, TyVar 2);
      ]

  let test_subst_exp =
    List.map
      (fun (l, x, t, u, e) -> l >:: fun _ -> assert_equal (subst_exp x t u) e)
      [
        "var", 1, TyInt, Var "x", Var "x";
        "fun", 2, TyInt, Fun (TyVar 1, "x", TyVar 2, Var "x"), Fun (TyVar 1, "x", TyInt, Var "x");
        "reset", 1, TyInt, Reset (Var "x", TyVar 1), Reset (Var "x", TyInt);
      ]
end

let suite = [
  "test_is_static_type">::: test_is_static_type;
  "test_tyvars">::: test_tyvars;
  "test_subst_type">::: CSR.test_subst_type;
  "test_subst_exp">::: CSR.test_subst_exp;
]
