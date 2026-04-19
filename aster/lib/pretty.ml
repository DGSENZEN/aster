open Format
open Ast

let str_of_binop ppf = function 
  | Add -> fprintf ppf "+"
  | Sub -> fprintf ppf "-"
  | Mul -> fprintf ppf "*"
  | Div -> fprintf ppf "/"

let str_of_comp ppf = function
  | Eq -> fprintf ppf "="
  | Lt -> fprintf ppf "<"
  | Gt -> fprintf ppf ">"
  | Le -> fprintf ppf "<="
  | Ge -> fprintf ppf ">="

let rec pp_expr ppf = function
  | Int i -> fprintf ppf "%d" i
  | Float f -> fprintf ppf "%f" f
  | Var s -> fprintf ppf "%s" s
  | Arith(l, op, r) -> fprintf ppf "@[<hv 1>%a %a %a@]" pp_expr l str_of_binop op pp_expr r
  | Comp(l, op, r) -> fprintf ppf "@[<hv 1>%a %a %a@]" pp_expr l str_of_comp op pp_expr r
  | Let(var, bind, body) -> fprintf ppf "@[<hv 1>let %s = %a in %a@]" var pp_expr bind pp_expr body
  | LetRec(var, bind, body) -> fprintf ppf "@[<hv 1>let rec %s = %a in %a@]" var pp_expr bind pp_expr body
  | Bool b -> fprintf ppf "@[<hv 1>%s @]" (if b then "true" else "false")
  | If(init_cond, t_branch, f_branch) -> fprintf ppf "@[<hv 1>if %a then %a else %a@]" pp_expr init_cond pp_expr t_branch pp_expr f_branch
  | Fun(param, body) -> fprintf ppf "@[<hv 1>fn %s -> (%a)@]" param pp_expr body
  | App(fn, arg) -> fprintf ppf "@[<hv 1>%a (%a)@]" pp_expr fn pp_expr arg
  | Tuple exprs -> fprintf ppf "(%a)" (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ")pp_expr) exprs
  | Match(expr, branches) -> fprintf ppf "@[<hv 1>match %a with %a@]" pp_expr expr (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") pp_branch) branches
and pp_pattern ppf = function 
  | PWild -> fprintf ppf "_"  
  | PInt i -> fprintf ppf "%d" i 
  | PVar x -> fprintf ppf "%s" x
  | PFloat f -> fprintf ppf "%f" f
  | PBool b -> fprintf ppf "%s" (if b = true then "true" else "false")
  | PTuple pats -> fprintf ppf "(%a)" (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_pattern) pats
and pp_branch ppf (pat, body) =
  fprintf ppf "| %a -> %a" pp_pattern pat pp_expr body
let expr_to_string e =
    asprintf "%a" pp_expr e 

let rec pp_value ppf = function
  | VInt i -> fprintf ppf "%d" i
  | VFloat f -> fprintf ppf "%f" f
  | VBool b -> fprintf ppf "%s" (if b then "true" else "false")
  | VClosure(param, body, captured_env) -> fprintf ppf "<fun>"
  | VTuple tups -> fprintf ppf "(%a)" (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_value ) tups

let value_to_string v = 
  asprintf "%a" pp_value v