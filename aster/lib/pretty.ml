open Format
open Ast

let rec pp_expr ppf = function
  | Int i -> fprintf ppf "%d" i
  | Var s -> fprintf ppf "%s" s
  | Add (l, r) -> 
    fprintf ppf "@[<hv 2>(%a@ + %a)@]" pp_expr l pp_expr r
  | Sub (l, r) -> 
    fprintf ppf "@[<hv 2>(%a@ - %a)@]" pp_expr l pp_expr r
  | Mul (l, r) ->
    fprintf ppf "@[<hv 2>(%a@ * %a)@]" pp_expr l pp_expr r
  | Div (l, r) ->
    fprintf ppf "@[<hv 2>(%a@ / %a)@]" pp_expr l pp_expr r 
  