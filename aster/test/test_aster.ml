open Aster.Pretty
open Aster.Eval
open Aster.Ast
open Format

(* 
let test = 
    Let ( "sum", Var("sum_n"), Add (Int 5, Int 12))

let test_let = Let ("x", Int 16, Add (Int 22, Int 16))
let test_let_two = Let ("x", Add (Var "x", Int 1), Var "x")
let test_add = Let ("Operator1", Int 16, Add(Int 21, Int 25))
let test_if = If (Bool(true), Let("x", Int 3, Add(Var "x", Int 2)), Bool(false))

let test_if_two = If(Bool(false), Div(Int 1, Int 0), Int 42)

let eval_test_add = eval [] test_add
let eval_test_if = eval [] test_if

let eval_test_if_two = eval [] test_if_two
let eval_test_let = eval [] test_let

(* TEST SUITE, these are the testing functions *)
let () = printf "%a@." pp_expr test_if
let () = printf "%a@." pp_value eval_test_if
let () = printf "%a@." pp_expr test_if_two
let () = printf "%a@." pp_value eval_test_if_two
(*let () = printf "Syntax: %a@." pp_expr test_let
let () = printf "Eval: %a@." pp_value eval_test_let*)
let () = printf "Syntax: %a@." pp_expr test_add
let () = printf "Evaluation: %a@." pp_value eval_test_add
(*let () = printf "Syntax: %a@." pp_expr test_let_two
let () = printf "Eval: %a@." pp_value eval_test_let_two*)

*)