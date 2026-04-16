open Format
open Ast
(*

NEED TO REFACTOR DUE TO THE NEW EVALUATOR.

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
  | Let (var, bind, body) ->
    fprintf ppf "@[<hv 2>let %s = %a in@ %a@]" var pp_expr bind pp_expr body
  | Bool b -> 
    fprintf ppf "@[<hv 2>%s @]" (if b then "true" else "false")
  | If (init_cond, t_branch, f_branch) ->
    fprintf ppf "@[<hv 1>if %a then %a@ else %a@]" pp_expr init_cond pp_expr t_branch pp_expr f_branch
  
let expr_to_string e =
    asprintf "%a" pp_expr e 

let pp_value ppf = function
  | VInt i -> fprintf ppf "%d" i
  | VBool b -> fprintf ppf "%s" (if b then "true" else "false")

let value_to_string v =
   asprintf "%a" pp_value v

*)