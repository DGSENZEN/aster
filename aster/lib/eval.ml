open Ast

let rec lookup name env = 
  match env with
  | [] -> failwith "There needs to be an existent variable in the environment!"
  | (k, v) :: rest -> if k = name then v else lookup name rest

let as_int = function
  | VInt i -> i
  | _ -> failwith "Not a valid integer value."

let as_bool = function 
  | VBool b -> b
  | _ -> failwith "Not a valid boolean value."


let comparison a b bbinop = let i = as_int(a) in let j = as_int(b) in 
match bbinop with 
| Eq -> VBool (i = j)
| Le -> VBool(i <= j)
| Lt -> VBool(i < j)
| Ge -> VBool(i >= j)
| Gt -> VBool(i > j)
| _ -> failwith "You need to use comparison operators for this to work."

let rec eval env = function
  | Int i -> VInt i
  | Var s -> lookup s env
  | Binop (l, bop, r) -> let i = eval env l 
  in let j = eval env r in (
    match bop with 
    | Add -> VInt(as_int(i) + as_int(j))
    | Sub -> VInt(as_int(i) - as_int(j))
    | Mul -> VInt(as_int(i) * as_int(j))
    | Div -> VInt(as_int(i) / as_int(j))
    | Eq | Le | Lt | Ge | Gt -> comparison i j bop
  )
  | Let (var, bind, body) -> 
    let bounded_val = eval env bind in
    let new_env = (var, bounded_val) :: env in
    eval new_env body
  | Bool b -> VBool b
  | If (init_cond, t_branch, f_branch) -> let eval_cond = eval env init_cond in
    (match eval_cond with
    | VBool true -> eval env t_branch
    | VBool false -> eval env f_branch
    | _ -> failwith "This is not a valid if construct!"
      )
  | Fun (param, body) -> VClosure(param, body, env)
  | App (fn, arg) -> let eval_fn = eval env fn in 
  let eval_arg = eval env arg in (
    match eval_fn with 
  | VClosure(param, body, captured_env) -> let new_env = (param, eval_arg) :: captured_env in eval new_env body
  | _ -> failwith "Not a valid function!"
  )