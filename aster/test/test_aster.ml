open Aster.Pretty
open Aster.Eval
open Aster.Ast
open Format

let new_eval_test = Arith(Int 6, Add, Int 7)
let () = printf "%a@." pp_expr new_eval_test
let new_eval_value_test = eval [] new_eval_test
let () = printf "%a@." pp_value new_eval_value_test


let float_eval_test = Arith(Float 6.0, Add, Float 7.1)
let () = printf "%a@." pp_expr float_eval_test
let float_eval_val_test = eval [] float_eval_test
let () = printf "%a@." pp_value float_eval_val_test

(* function testing 
(fun x -> x + 1) 5 *)

let func_test = Fun("x", Arith(Var "x", Add, Int 1))
let () = printf "%a@." pp_expr func_test
let func_app_test = App(func_test, Int 5)
let func_app_eval = eval [] func_app_test
let () = printf "%a@." pp_expr func_app_test
let () = printf "%a@." pp_value func_app_eval

let closure_test = Let("a", Int 1, Let("f", Fun("x", Arith(Var("x"), Add, Var("a"))), App(Var("f"), Int 5)))
let closure_eval_test = eval [] closure_test
let () = printf "%a@." pp_expr closure_test
let () = printf "%a@." pp_value closure_eval_test

let closure_test_two = Let("a", Int 1, Let("f", Fun("x", Arith(Var("x"), Add, Var("a"))), Let("a", Int 100, App(Var("f"), Int 5))))
let closure_eval_test_two = eval [] closure_test_two
let () = printf "%a@." pp_expr closure_test_two
let () = printf "%a@." pp_value closure_eval_test_two

let fact_test = LetRec("fact", Fun("n", If(Comp(Var("n"), Eq, Int 0), Int 1, Arith(Var("n"), Mul, App(Var("fact"), Arith(Var("n"), Sub, Int 1))))), App(Var("fact"), Int 5))
let () = printf "%a@." pp_expr fact_test
let fact_test_eval = eval [] fact_test
let () = printf "%a@." pp_value fact_test_eval

let fib_test = LetRec("fib", Fun("n", If(Comp(Var("n"), Lt, Int 2), Var("n"), Arith(App(Var("fib"), Arith(Var("n"), Sub, Int 1)), Add, App(Var("fib"), Arith(Var("n"), Sub, Int 2))))), App(Var("fib"), Int 10))
let () = printf "%a@." pp_expr fib_test
let eval_fib_test = eval [] fib_test
let () = printf "%a@." pp_value eval_fib_test

(* match 5 with x -> x + 1 *)
let match_test1 = Match(Int 5, [(PVar "x", Arith(Var "x", Add, Int 1))])
let () = printf "%a@." pp_expr match_test1
let eval_match_test1 = eval [] match_test1
let () = printf "%a@." pp_value eval_match_test1
(* should give VInt 6 *)

(* match 2 with | 1 -> 10 | 2 -> 20 | _ -> 0 *)
let match_test2 = Match(Int 2, [
  (PInt 1, Int 10);
  (PInt 2, Int 20);
  (PWild, Int 0)
])

let () = printf "%a@." pp_expr match_test2

let eval_match_test2 = eval [] match_test2
let () = printf "%a@." pp_value eval_match_test2

(* should give VInt 20 *)

(* match (3, 4) with (x, y) -> x + y *)
let match_test3 = Match(Tuple [Int 3; Int 4], [(PTuple [PVar "x"; PVar "y"], Arith(Var "x", Add, Var "y"))])
(* should give VInt 7 *)
let () = printf "%a@." pp_expr match_test3

let eval_match_test3 = eval [] match_test3

let () = printf "%a@." pp_value eval_match_test3
(* match (1, 2, 3) with (_, y, _) -> y *)
let match_test4 = Match(Tuple [Int 1; Int 2; Int 3], [(PTuple [PWild; PVar "y"; PWild], Var "y")])

let () = printf "%a@." pp_expr match_test4

let eval_match_test_4 = eval [] match_test4
let () = printf "%a@." pp_value eval_match_test_4
(* should give VInt 2 *)
