                                   open Base
                                  open Parser
                                   open Stdio
                                   open Types

                         (* Convert Sexp.t to value *)
                           let rec of_sexp = function
                                  | Atom s ->
                        (match Int.of_string_opt s with
                             | Some n -> Integer n
                                   | None ->
                       (match Float.of_string_opt s with
                              | Some n -> Float n
                                   | None ->
                                 (match s with
                             | "real" -> Bool true
                             | "cap" -> Bool false
                                     | _ ->
                                       if
                              String.length s > 2
                       && Char.equal (String.get s 0) '"'
             && Char.equal (String.get s (String.length s - 1)) '"'
          then String (String.sub s ~pos:1 ~len:(String.length s - 2))
                                else Symbol s)))
                 | SexpList xs -> List (List.map ~f:of_sexp xs)
                                       ;;

             (* Create initial environment with basic operations *)
                          let create_initial_env () =
                  let ctx = Hashtbl.create (module String) in
                             (* type predicates *)
                              let mk_tp checker =
                                    Function
                                   (function
                          | [ x ] -> Bool (checker x)
         | _ -> "Type predicate expects 1 (one) argument" |> failwith)
                                       in
                                  Hashtbl.set
                                      ctx
                                ~key:"integer?"
                                     ~data:
                                (mk_tp (function
                              | Integer _ -> true
                                | _ -> false));
                                  Hashtbl.set
                                      ctx
                                 ~key:"float?"
                                     ~data:
                                (mk_tp (function
                               | Float _ -> true
                                | _ -> false));
                                  Hashtbl.set
                                      ctx
                                 ~key:"symbol?"
                                     ~data:
                                (mk_tp (function
                               | Symbol _ -> true
                                | _ -> false));
                                  Hashtbl.set
                                      ctx
                                 ~key:"string?"
                                     ~data:
                                (mk_tp (function
                               | String _ -> true
                                | _ -> false));
                                  Hashtbl.set
                                      ctx
                                  ~key:"list?"
                                     ~data:
                                (mk_tp (function
                                | List _ -> true
                                | _ -> false));
                            let nil_pred = function
                           | [ List [] ] -> Bool true
                           | [ List _ ] -> Bool false
          | _ -> "'nil?' expects an argument of type list" |> failwith
                                       in
             Hashtbl.set ctx ~key:"nil?" ~data:(Function nil_pred);
   let fmod x y = Float.of_int (Int.rem (Int.of_float x) (Int.of_float y)) in
                    let add_primitive name int_op float_op =
                               let bin = function
               | [ Integer a; Integer b ] -> Integer (int_op a b)
        | [ Integer a; Float b ] -> Float (float_op (Float.of_int a) b)
        | [ Float a; Integer b ] -> Float (float_op a (Float.of_int b))
                 | [ Float a; Float b ] -> Float (float_op a b)
       | _ -> Invalid_argument ("Invalid arguments to " ^ name) |> raise
                                       in
                 Hashtbl.set ctx ~key:name ~data:(Function bin)
                                       in
                       let add_cmp name int_op float_op =
                               let cmp = function
                | [ Integer a; Integer b ] -> Bool (int_op a b)
         | [ Integer a; Float b ] -> Bool (float_op (Float.of_int a) b)
         | [ Float a; Integer b ] -> Bool (float_op a (Float.of_int b))
                 | [ Float a; Float b ] -> Bool (float_op a b)
       | _ -> Invalid_argument ("Invalid arguments to " ^ name) |> raise
                                       in
                 Hashtbl.set ctx ~key:name ~data:(Function cmp)
                                       in
                let add_cmp_bool name int_op float_op bool_op =
                               let cmp = function
                 | [ Float a; Float b ] -> Bool (float_op a b)
                | [ Integer a; Integer b ] -> Bool (int_op a b)
         | [ Integer a; Float b ] -> Bool (float_op (Float.of_int a) b)
         | [ Float a; Integer b ] -> Bool (float_op a (Float.of_int b))
                   | [ Bool a; Bool b ] -> Bool (bool_op a b)
       | _ -> Invalid_argument ("Invalid arguments to " ^ name) |> raise
                                       in
                 Hashtbl.set ctx ~key:name ~data:(Function cmp)
                                       in
                       add_cmp ">" Int.( > ) Float.( > );
                       add_cmp "<" Int.( < ) Float.( < );
                     add_cmp "<>" Int.( <> ) Float.( <> );
                     add_cmp ">=" Int.( >= ) Float.( >= );
                     add_cmp "<=" Int.( <= ) Float.( <= );
                        add_primitive "+" ( + ) ( +. );
                        add_primitive "%" Int.rem fmod;
                        add_primitive "*" ( * ) ( *. );
                        add_primitive "/" ( / ) ( /. );
                        add_primitive "-" ( - ) ( -. );
                       add_primitive "^" ( ** ) ( **. );
               add_cmp_bool "=" Int.( = ) Float.( = ) Bool.( = );
             add_cmp_bool "<>" Int.( <> ) Float.( <> ) Bool.( <> );
                             let cons_op = function
                       | [ x; List xs ] -> List (x :: xs)
                          | [ x; y ] -> List [ x; y ]
                 | _ -> "Invalid arguments to cons" |> failwith
                                       in
             Hashtbl.set ctx ~key:"cons" ~data:(Function cons_op);
                             let car_op = function
                            | [ List (x :: _) ] -> x
       | [ List [] ] -> "car being applied to an empty List" |> failwith
                  | _ -> "Invalid argument to car" |> failwith
                                       in
              Hashtbl.set ctx ~key:"car" ~data:(Function car_op);
                             let cdr_op = function
                        | [ List (_ :: xs) ] -> List xs
       | [ List [] ] -> "cdr being applied to an empty List " |> failwith
                  | _ -> "Invalid Argument to cdr" |> failwith
                                       in
              Hashtbl.set ctx ~key:"cdr" ~data:(Function cdr_op);
                            let append_op = function
               | [ List y'; List x' ] -> List (List.append x' y')
                    | [ y'; List x' ] -> List (x' @ [ y' ])
| _ -> "append (&) expects two arguments (List, List) or (List, value)" |> failwith
                                       in
           Hashtbl.set ctx ~key:"append" ~data:(Function append_op);
              Hashtbl.set ctx ~key:"&" ~data:(Function append_op);
                            let length_op = function
                   | [ List x' ] -> Integer (List.length x')
          | _ -> "Length expects an argument of type List" |> failwith
                                       in
           Hashtbl.set ctx ~key:"length" ~data:(Function length_op);
                             let map_op = function
     | [ Function f; List x' ] -> List (List.map ~f:(fun x -> f [ x ]) x')
    | _ -> "Map expects 2 arguments of type (function λ, list)" |> failwith
                                       in
              Hashtbl.set ctx ~key:"map" ~data:(Function map_op);
                            let filter_op = function
                          | [ Function f; List x' ] ->
                                let truthy arg =
                                 match arg with
                              | Bool true -> true
                                  | _ -> false
                                       in
              List (List.filter ~f:(fun x -> truthy (f [ x ])) x')
   | _ -> "Filter expects 2 argumetns of type (function λ, list)" |> failwith
                                       in
           Hashtbl.set ctx ~key:"filter" ~data:(Function filter_op);
                            let foldl_op = function
                       | [ init; List x'; Function f ] ->
             List.fold_left x' ~init ~f:(fun acc x -> f [ acc; x ])
  | _ -> "Incorrect usage of foldl: foldl <acc> <list> <function>" |> failwith
                                       in
            Hashtbl.set ctx ~key:"foldl" ~data:(Function foldl_op);
                            let foldr_op = function
                       | [ List x'; init; Function f ] ->
            List.fold_right x' ~init ~f:(fun acc x -> f [ x; acc ])
  | _ -> "Incorrect usage of foldr: foldl <list> <acc> <function>" |> failwith
                                       in
            Hashtbl.set ctx ~key:"foldr" ~data:(Function foldr_op);
                            let sort_asc = function
          | [ List x' ] -> List (List.sort x' ~compare:Stdlib.compare)
| _ -> "Incorrect usage of sort_asc: Only accepts arguments of type List" |> failwith
                                       in
           Hashtbl.set ctx ~key:"sort_asc" ~data:(Function sort_asc);
                            let sort_desc = function
    | [ List x' ] -> List (List.rev (List.sort x' ~compare:Stdlib.compare))
| _ -> "Incorrect usage of sort_desc: Only accepts arguments of type List" |> failwith
                                       in
          Hashtbl.set ctx ~key:"sort_desc" ~data:(Function sort_desc);
                              let getn = function
                          | [ Integer n; List x' ] ->
                           (match List.nth x' n with
                                 | Some x -> x
                                   | None ->
                              "Invalid n value '"
                            ^ Stdlib.string_of_int n
             ^ "' for 'get n <list>'. Maximum bound for list is '"
                  ^ Stdlib.string_of_int (List.length x' - 1)
                                     ^ "'"
                                  |> failwith)
            | _ -> "Invalid argument(s) to get n <list>" |> failwith
                                       in
               Hashtbl.set ctx ~key:"get" ~data:(Function getn);
                              let setn = function
             | [ Integer n; List x'; v ] when List.length x' > n ->
 let replace l pos a = List.mapi l ~f:(fun i x -> if i = pos then a else x) in
                             List (replace x' n v)
                         | [ Integer n; List x'; _ ] ->
                              "Invalid n value '"
                            ^ Stdlib.string_of_int n
         ^ "' for 'set n <list> <value>'. Maximum bound for list is '"
                  ^ Stdlib.string_of_int (List.length x' - 1)
                                     ^ "'"
                                  |> failwith
            | _ -> "Invalid argument(s) to get n <list>" |> failwith
                                       in
               Hashtbl.set ctx ~key:"set" ~data:(Function setn);
                    let list_constructor args = List args in
         Hashtbl.set ctx ~key:"list" ~data:(Function list_constructor);
          Hashtbl.set ctx ~key:"@" ~data:(Function list_constructor);
                  Hashtbl.set ctx ~key:"nil" ~data:(List []);
                                      ctx
                                       ;;

                  (* Evaluate a value in the given context *)

                          let rec eval ctx = function
                               | Bool b -> Bool b
                            | Integer n -> Integer n
                              | Float n -> Float n
                             | String s -> String s
                                 | Symbol s ->
                         (match Hashtbl.find ctx s with
                                 | Some v -> v
                | None -> "Undefined symbol: " ^ s |> failwith)
                              | List [] -> List []
             | List [ Symbol "if"; cond; then_expr; else_expr ] ->
                           (match eval ctx cond with
                       | Bool true -> eval ctx then_expr
                       | Bool false -> eval ctx else_expr
            | _ -> "Invalid condition in if expression" |> failwith)
                 | List [ Symbol "let"; Symbol name; expr ] ->
                          let value = eval ctx expr in
                     Hashtbl.set ctx ~key:name ~data:value;
                                     value
                  | List [ Symbol "$"; List params; body ] ->
                               let param_names =
                                    List.map
                                  ~f:(function
                             | Symbol name -> name
                  | _ -> "Invalid Parameter anme" |> failwith)
                                     params
                                       in
                                    Function
                                  (fun args ->
                 if List.length args <> List.length param_names
               then "Argument count mismatch dumbass" |> failwith
                                     else (
                       let tmp_ctx = Hashtbl.copy ctx in
              List.iter2_exn param_names args ~f:(fun key value ->
                     Hashtbl.set tmp_ctx ~key ~data:value);
                              eval tmp_ctx body))
                             | List (fn :: args) ->
                          let fn_val = eval ctx fn in
                 let arg_vals = List.map ~f:(eval ctx) args in
                               (match fn_val with
                           | Function f -> f arg_vals
                      | _ -> "Not a function" |> failwith)
                    | _ -> "Invalid expression" |> failwith
                                       ;;

                       (* Parse and evaluate a string *)
                           let eval_string ctx str =
                          let tokens = tokenize str in
                let s_exp = s_expression_of_token_list tokens in
                         let values = of_sexp s_exp in
                                eval ctx values
                                       ;;

                      (* Pretty printer for Lisp values *)
                       let rec string_of_value = function
                         | Integer n -> Int.to_string n
                         | Float n -> Float.to_string n
                                  | Bool b ->
                                 (match b with
                                | true -> "real"
                               | false -> "cap")
                          | String s -> "'" ^ s ^ "'"
                  | Symbol s -> s (* implement cons dumbass *)
| List vs -> "(" ^ String.concat ~sep:" " (List.map ~f:string_of_value vs) ^ ")"
                         | Function _ -> "<function λ>"
                                       ;;

         (* Read a complete s-expression, handling multi-line input *)
                          let read_complete_expr () =
                        (* modify this because buggy *)
                           let open_parens = ref 0 in
                          let close_parens = ref 0 in
                       let buffer = Buffer.create 256 in
                        let rec read_lines first_line =
             printf "%s" (if first_line then " λ> " else "... > ");
                           Out_channel.flush stdout;
               match In_channel.input_line In_channel.stdin with
                            | None -> None (* EOF *)
                                 | Some line ->
                               (* Count parens *)
                         String.iter line ~f:(function
                         | '(' -> Int.incr open_parens
                         | ')' -> Int.incr close_parens
                                  | _ -> ());
                         Buffer.add_string buffer line;
                          Buffer.add_char buffer ' ';
                        if !open_parens = !close_parens
                       then Some (Buffer.contents buffer)
                             else read_lines false
                                       in
                                read_lines true
                                       ;;

                              (* REPL main loop *)

                               let help_message =
                          "Usage: lisexp [options] \n\
                                     \ \n\
                A REPL for a Lisp dialect written in OCaml. \n\
                                 Commands: \n\
\  :env                - Prints all user-defined and built-in functions and variables\n\
 \  :env <var>          - Prints the value of the specified variable <var> \n\
             \  :help               - Displays this help message\n\
                 \  :wq                 - Exits the REPL \n\n\
                             Core Expressions: \n\
               \  - Arithmetic: +, -, *, /, ^ (Exponentiation)\n\
         \  - Comparison: >, <, >=, <=, = (for numbers and strings) \n\
    \  - Type Predicates: integer?, float?, symbol?, string?, list?, nil?\n\
            \  - Control Flow: if (<condition> <then> <else>) \n\n\
                            Example Expressions: \n\
                        \  > (+ 1 2)             => 3\n\
                       \  > (if (< 1 2) 1 0)    => 1 \n\
                         \  > (let x 42)       => 42\n\
\  > (let fact ($ (n) (if (= n 0) 1 (* n (fact (- n 1))))))  => <function λ> \n\n\
                             Lambda Functions: \n\
      \  - Define a function using (let <name> ($ (<param>) (<body>)))\n\
                               \  - Example: \n\
       \      > (let fact ($ (n) (if (= n 0) 1 (* n (fact (- n 1))))))\n\
                          \      => <function λ> \n\n\
                       Built-in Functions for Lists: \n\
  \  - cons: Constructs a new list from an element and the rest of the list\n\
               \  - car: Returns the first element of a list \n\
       \  - cdr: Returns the rest of the list after the first element\n\
     \  - append (&): Concatenates a list or adds an element to a list \n\
                  \  - length: Returns the length of a list\n\
           \  - map: Applies a function to each element of a list \n\
              \  - filter: Filters a list based on a predicate\n\
                           \  - foldl: Left fold \n\
                           \  - foldr: Right fold\n\
               \  - sort_asc: Sorts a list in ascending order \n\
              \  - sort_desc: Sorts a list in descending order\n\
            \  - get: Retrieves an element from a list by index \n\
              \  - set: Modifies an element in a list by index\n\
                                     \ \n\
                          Example List Operations:\n\
          \  > (let x (cons 1 (cons 2 (cons 3 nil))))  => (1 2 3) \n\
                             \  > (car x)  => 1\n\
                           \  > (cdr x)  => (2 3) \n\
                       \  > (append 4 x)  => (1 2 3 4)\n\
                           \  > (length x)  => 3 \n\
              \  > (map ($ (x) (+ x 1)) (@ 1 2 3))  => (2 3 4)\n\
         \  > (filter ($ (x) (< x 10)) (@ 8 9 10 11 12))  => (8 9) \n\
              \  > (foldl 0 (@ 1 2 3) ($ (x y) (+ x y)))  => 6\n\
                   \  > (sort_asc (@ 3 2 1))  => (1 2 3) \n\
                        \  > (get 0 (@ 1 2 3))  => 1\n\
                  \  > (set 0 (@ 1 2 3) 100)  => (100 2 3) \n\
                                   Notes: \n\
\  - The REPL supports basic Lisp-like syntax with arithmetic, comparison operators, \
                               and functions.\n\
               inspect the available functions and variables. \n\
               \  - Use (let) to bind variables or functions.\n\
       \  - Lambda functions are defined with ($ (<params>) <body>). \n\
            \  - Control flow can be managed with if statements.\n\
                                    Exit:\n\
                      \  - Type ':wq' to exit the REPL.\n"
                                       ;;

                               let run_repl () =
                       let env = create_initial_env () in
                           printf "Mini-Lisp REPL\n";
                         printf "Type ':wq' to exit\n";
                     printf "Type ':help' for commands\n";
printf "Type ':env <option>' for values (functions inclusive) in the environment\n";
                               let rec loop () =
                        match read_complete_expr () with
                        | None -> printf "\nGoodbye!\n"
                                | Some input ->
                      let trimmed = String.strip input in
                              (match trimmed with
                                | "" -> loop ()
                           | ":wq" -> printf ":wq!\n"
                                  | ":help" ->
                          print_endline help_message;
                                    loop ()
                                  | ":env" ->
                       printf "\nCurrent environment:\n";
                    Hashtbl.iteri env ~f:(fun ~key ~data ->
                printf "  %s: %s\n" key (string_of_value data));
                                  printf "\n";
                                    loop ()
                                    | input
                          when String.length input > 5
          && String.equal (String.sub input ~pos:0 ~len:5) ":env " ->
      let key = String.sub input ~pos:5 ~len:(String.length input - 5) in
                        (match Hashtbl.find env key with
             | Some v -> printf "%s: %s\n" key (string_of_value v)
                | None -> printf "'%s' not found in env\n" key);
                                    loop ()
                                     | _ ->
                                      (try
                       let start = Stdlib.Sys.time () in
                    let result = eval_string env trimmed in
                        let end' = Stdlib.Sys.time () in
                                     printf
                             " => %s\n\tin %fms\n"
                            (string_of_value result)
                          ((end' -. start) *. 1000.);
                                    loop ()
                                      with
                                | Failure msg ->
                           printf "Error: %s\n" msg;
                                    loop ()
                                     | e ->
               printf "Unexpected error: %s\n" (Exn.to_string e);
                                   loop ()))
                                       in
                                    loop ()
                                       ;;
