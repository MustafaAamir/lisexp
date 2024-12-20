open Sexp
open Stdio

let () =
  let env = create_initial_env () in

  (* Test some expressions *)
  let test expr =
    printf "Expression: %s\n" expr;
    match eval_string env expr with
    | Number n -> printf "Result: %d\n" n
    | Bool b -> printf "Result: %b\n" b
    | _ -> printf "Result: <complex value>\n"
  in

  test "(+ 1 2)";
  test "(* 3 4)";
  test "(= 5 5)";
  test "(if (< 1 2) 10 20)";
  test "(define x 42)";
  test "x"
