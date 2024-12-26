open Lisp
open OUnit2
open Base

(* Helper functions for testing *)
let test_eval env input expected =
  let result = eval_string env input in
  assert_equal ~printer:string_of_value expected result

let setup_test_env () = fst (create_initial_env ())

(* Test Suites *)
let value_tests =
  "value_tests"
  >::: [
         ( "test_integer" >:: fun _ ->
           let env = setup_test_env () in
           test_eval env "42" (Integer 42) );
         ( "test_float" >:: fun _ ->
           let env = setup_test_env () in
           test_eval env "3.14" (Float 3.14) );
         ( "test_bool" >:: fun _ ->
           let env = setup_test_env () in
           test_eval env "real" (Bool true);
           test_eval env "fake" (Bool false) );
         ( "test_string" >:: fun _ ->
           let env = setup_test_env () in
           test_eval env "'hello'" (String "hello") );
         ( "test_list" >:: fun _ ->
           let env = setup_test_env () in
           test_eval env "(list 1 2 3)"
             (List [ Integer 1; Integer 2; Integer 3 ]) );
       ]

let arithmetic_tests =
  "arithmetic_tests"
  >::: [
         ( "test_addition" >:: fun _ ->
           let env = setup_test_env () in
           test_eval env "(+ 1 2)" (Integer 3);
           test_eval env "(+ 1.5 2.5)" (Float 4.0) );
         ( "test_subtraction" >:: fun _ ->
           let env = setup_test_env () in
           test_eval env "(- 5 3)" (Integer 2);
           test_eval env "(- 5.5 3.3)" (Float 2.2) );
         ( "test_multiplication" >:: fun _ ->
           let env = setup_test_env () in
           test_eval env "(* 4 3)" (Integer 12);
           test_eval env "(* 2.5 2)" (Float 5.0) );
         ( "test_division" >:: fun _ ->
           let env = setup_test_env () in
           test_eval env "(/ 10 2)" (Integer 5);
           test_eval env "(/ 5.0 2)" (Float 2.5) );
       ]

let comparison_tests =
  "comparison_tests"
  >::: [
         ( "test_equal" >:: fun _ ->
           let env = setup_test_env () in
           test_eval env "(= 1 1)" (Bool true);
           test_eval env "(= 1 2)" (Bool false);
           test_eval env "(= 1.0 1.0)" (Bool true) );
         ( "test_less_than" >:: fun _ ->
           let env = setup_test_env () in
           test_eval env "(< 1 2)" (Bool true);
           test_eval env "(< 2 1)" (Bool false);
           test_eval env "(< 1.0 2.0)" (Bool true) );
         ( "test_greater_than" >:: fun _ ->
           let env = setup_test_env () in
           test_eval env "(> 2 1)" (Bool true);
           test_eval env "(> 1 2)" (Bool false);
           test_eval env "(> 2.0 1.0)" (Bool true) );
       ]

let list_operation_tests =
  "list_operation_tests"
  >::: [
         ( "test_car" >:: fun _ ->
           let env = setup_test_env () in
           test_eval env "(car (list 1 2 3))" (Integer 1) );
         ( "test_cdr" >:: fun _ ->
           let env = setup_test_env () in
           test_eval env "(cdr (list 1 2 3))" (List [ Integer 2; Integer 3 ]) );
         ( "test_cons" >:: fun _ ->
           let env = setup_test_env () in
           test_eval env "(cons 1 (list 2 3))"
             (List [ Integer 1; Integer 2; Integer 3 ]) );
         ( "test_append" >:: fun _ ->
           let env = setup_test_env () in
           test_eval env "(append (list 1 2) (list 3 4))"
             (List [ Integer 3; Integer 4; Integer 1; Integer 2 ]) );
       ]

let function_tests =
  "function_tests"
  >::: [
         ( "test_lambda_creation" >:: fun _ ->
           let env = setup_test_env () in
           let _ = eval_string env "(define add ($ (x y) (+ x y)))" in
           test_eval env "(add 2 3)" (Integer 5) );
         ( "test_lambda_closure" >:: fun _ ->
           let env = setup_test_env () in
           let _ = eval_string env "(define x 1)" in
           let _ = eval_string env "(define f ($ (y) (+ x y)))" in
           let _ = eval_string env "(define x 10)" in
           test_eval env "(f 2)" (Integer 12) );
         ( "test_recursive_function" >:: fun _ ->
           let env = setup_test_env () in
           let _ =
             eval_string env
               "\n\
               \      (define factorial\n\
               \        ($ (n)\n\
               \           (if (= n 0)\n\
               \               1\n\
               \               (* n (factorial (- n 1))))))"
           in
           test_eval env "(factorial 5)" (Integer 120) );
       ]

let higher_order_function_tests =
  "higher_order_function_tests"
  >::: [
         ( "test_map" >:: fun _ ->
           let env = setup_test_env () in
           let _ = eval_string env "(define double ($ (x) (* x 2)))" in
           test_eval env "(map double (list 1 2 3))"
             (List [ Integer 2; Integer 4; Integer 6 ]) );
         ( "test_filter" >:: fun _ ->
           let env = setup_test_env () in
           let _ =
             eval_string env "(define even? ($ (x) (= (/ x 2) (* (/ x 2.0)))))"
           in
           test_eval env "(filter even? (list 1 2 3 4))"
             (List [ Integer 2; Integer 4 ]) );
         ( "test_foldl" >:: fun _ ->
           let env = setup_test_env () in
           test_eval env "(foldl 0 (list 1 2 3) +)" (Integer 6) );
       ]

let error_handling_tests =
  "error_handling_tests"
  >::: [
         ( "test_undefined_symbol" >:: fun _ ->
           let env = setup_test_env () in
           assert_raises (Failure "Undefined symbol: xyz") (fun () ->
               eval_string env "xyz") );
         ( "test_type_error" >:: fun _ ->
           let env = setup_test_env () in
           assert_raises (Failure "Invalid arguments to +") (fun () ->
               eval_string env "(+ 1 'hello')") );
         ( "test_arity_error" >:: fun _ ->
           let env = setup_test_env () in
           let _ = eval_string env "(define f ($ (x) x))" in
           assert_raises (Failure "Argument count mismatch dumbass") (fun () ->
               eval_string env "(f 1 2)") );
       ]

(* Run all tests *)
let suite =
  "all_tests"
  >::: [
         value_tests;
         arithmetic_tests;
         comparison_tests;
         list_operation_tests;
         function_tests;
         higher_order_function_tests;
         error_handling_tests;
       ]

let () = run_test_tt_main suite
