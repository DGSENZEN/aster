open Aster.Ast
open Aster.Pretty
open Format

let test = 
    Add (Int 5, Int 12)

let () = printf "%a@." pp_expr test