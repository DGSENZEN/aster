open Ast

type env = (string * int) list

let rec lookup name env = 
  match env with
  | [] -> failwith "There needs to be an existent variable in the environment!"
  | (k, v) :: rest -> if k = name then v else lookup name rest

let rec eval env = function
  | Int i -> i
  | Var s -> lookup s env
  | Add (l, r) -> eval env l + eval env r
  | Sub (l, r) -> eval env l - eval env r
  | Mul (l, r) -> eval env l * eval env r
  | Div (l, r) -> eval env l / eval env r
  | Let (var, bind, body) -> 
    let bounded_val = eval env bind in
    let new_env = (var, bounded_val) :: env in
    eval new_env body