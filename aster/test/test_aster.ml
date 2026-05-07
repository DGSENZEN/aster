open Aster

let fail_test name message =
  Printf.eprintf "FAIL %s: %s\n" name message;
  exit 1

let check_value name expr expected =
  try
    let actual = Eval.eval Eval.empty_env expr in
    if actual = expected then
      Printf.printf "ok   %s\n" name
    else
      fail_test name
        (Printf.sprintf
           "expected %s, got %s"
           (Pretty.value_to_string expected)
           (Pretty.value_to_string actual))
  with
  | Error.Eval_error err ->
      fail_test name (Error.error_to_string err)

let check_error name expr =
  try
    let actual = Eval.eval Eval.empty_env expr in
    fail_test name
      (Printf.sprintf "expected error, got %s" (Pretty.value_to_string actual))
  with
  | Error.Eval_error _ ->
      Printf.printf "ok   %s\n" name

let () =
  let open Ast in
  check_value
    "arithmetic precedence via AST"
    (add (int 1) (mul (int 2) (int 3)))
    (VInt 7);

  check_value
    "let name"
    (let_name "x" (int 41) (add (var "x") (int 1)))
    (VInt 42);

  check_value
    "tuple pattern let"
    (let_
       (PTuple [PVar "x"; PVar "y"])
       (tuple [int 20; int 22])
       (add (var "x") (var "y")))
    (VInt 42);

  check_value
    "function application"
    (app (fun_ "x" (add (var "x") (int 1))) (int 41))
    (VInt 42);

  check_value
    "unit literal"
    unit
    (VUnit);

  check_value
    "unit pattern let"
    (let_ PUnit unit (int 42))
    (VInt 42);

  check_value
    "unit pattern match"
    (match_ unit [(PUnit, int 42)])
    (VInt 42);

  check_value
    "string literal"
    (string "hello")
    (VString "hello");

  check_value
    "string pattern match"
    (match_
       (string "hi")
       [(PString "hi", string "bye")])
    (VString "bye");

  check_value
    "string equality"
    (eq (string "same") (string "same"))
    (VBool true);

  check_value
    "multi-arm match skips failed arms"
    (match_
       (int 2)
       [
         (PInt 1, int 10);
         (PInt 2, int 20);
         (PWild, int 0);
       ])
    (VInt 20);

  check_value
    "tuple projection with wildcards"
    (match_
       (tuple [int 1; int 2; int 3])
       [(PTuple [PWild; PVar "y"; PWild], var "y")])
    (VInt 2);

  check_value
    "closure captures lexical environment"
    (let_name
       "a"
       (int 1)
       (let_name
          "f"
          (fun_ "x" (add (var "x") (var "a")))
          (app (var "f") (int 5))))
    (VInt 6);

  check_value
    "closure ignores later shadowing"
    (let_name
       "a"
       (int 1)
       (let_name
          "f"
          (fun_ "x" (add (var "x") (var "a")))
          (let_name "a" (int 100) (app (var "f") (int 5)))))
    (VInt 6);

  let fact_body =
    if_
      (eq (var "n") (int 0))
      (int 1)
      (mul
         (var "n")
         (app (var "fact") (sub (var "n") (int 1))))
  in
  check_value
    "let rec factorial"
    (let_rec "fact" "n" fact_body (app (var "fact") (int 5)))
    (VInt 120);

  check_value
    "match tuple"
    (match_
       (tuple [int 1; bool true])
       [
         (PTuple [PInt 0; PWild], int 0);
         (PTuple [PVar "x"; PBool true], add (var "x") (int 41));
       ])
    (VInt 42);

  check_error
    "division by zero"
    (div (int 1) (int 0));

  check_error
    "bad application"
    (app (int 1) (int 2));

  check_error
    "closure equality regression"
    (eq (fun_ "x" (var "x")) (fun_ "x" (var "x")));

  check_error
    "closure equality asymmetry regression"
    (eq (fun_ "x" (var "x")) (int 1));

  check_error
    "tuple closure equality regression"
    (eq
       (tuple [fun_ "x" (var "x")])
       (tuple [fun_ "x" (var "x")]));

  check_error
    "duplicate bindings"
    (let_
       (PTuple [PVar "x"; PVar "x"])
       (tuple [int 1; int 2])
       (var "x"));

  Printf.printf "\nAll tests passed.\n";
  Printf.printf "Brooooo...."
