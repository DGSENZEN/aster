open Aster.Ast
open Aster.Pretty
open Format

let test = 
    Let ( "sum", Var("sum_n"), Add (Int 5, Int 12))

let test_let = Let ("x", Int 16, Add (Var "y", Int 16))

let () = printf "%a@." pp_expr test
let () = printf "%a@." pp_expr test_let