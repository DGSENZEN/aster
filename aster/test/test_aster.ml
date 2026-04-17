open Aster.Pretty
open Aster.Eval
open Aster.Ast
open Format

let new_eval_test = Binop(Int 6, Add, Int 7)
let () = printf "%a@." pp_expr new_eval_test
let new_eval_value_test = eval [] new_eval_test
let () = printf "%a@." pp_value new_eval_value_test


let float_eval_test = Binop(Float 6.0, Add, Float 7.1)
let () = printf "%a@." pp_expr float_eval_test
let float_eval_val_test = eval [] float_eval_test
let () = printf "%a@." pp_value float_eval_val_test

(* function testing 
(fun x -> x + 1) 5 *)

let func_test = Fun("x", Binop(Var "x", Add, Int 1))
let () = printf "%a@." pp_expr func_test
let func_app_test = App(func_test, Int 5)
let func_app_eval = eval [] func_app_test
let () = printf "%a@." pp_expr func_app_test
let () = printf "%a@." pp_value func_app_eval

let closure_test = Let("a", Int 1, Let("f", Fun("x", Binop(Var("x"), Add, Var("a"))), App(Var("f"), Int 5)))
let closure_eval_test = eval [] closure_test
let () = printf "%a@." pp_expr closure_test
let () = printf "%a@." pp_value closure_eval_test

let closure_test_two = Let("a", Int 1, Let("f", Fun("x", Binop(Var("x"), Add, Var("a"))), Let("a", Int 100, App(Var("f"), Int 5))))
let closure_eval_test_two = eval [] closure_test_two
let () = printf "%a@." pp_expr closure_test_two
let () = printf "%a@." pp_value closure_eval_test_two

let fact_test = LetRec("fact", Fun("n", If(Binop(Var("n"), Eq, Int 0), Int 1, Binop(Var("n"), Mul, App(Var("fact"), Binop(Var("n"), Sub, Int 1))))), App(Var("fact"), Int 5))
let () = printf "%a@." pp_expr fact_test
let fact_test_eval = eval [] fact_test
let () = printf "%a@." pp_value fact_test_eval