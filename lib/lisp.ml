open Base
open Parser
open Stdio

type value =
  | Integer of int
  | Float of float
  | Symbol of string
  | String of string
  | Bool of bool
  | List of value list
  | Function of (value list -> value)

(* Environment to store variables and functions *)
type ctx = (string, value) Hashtbl.t
type signature = (string, string) Hashtbl.t

(* Convert Sexp.t to value *)
let rec of_sexp = function
  | Atom s -> (
      match Int.of_string_opt s with
      | Some n -> Integer n
      | None -> (
          match Float.of_string_opt s with
          | Some n -> Float n
          | None -> (
              match s with
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

(* Create initial environment with basic operations *)
let create_initial_env () =
  let ctx = Hashtbl.create (module String) in
  let signature = Hashtbl.create (module String) in

  (* type predicates *)
  let mk_tp checker =
    Function
      (function
      | [ x ] -> Bool (checker x)
      | _ -> "Type predicate expects 1 (one) argument" |> failwith)
  in

  Hashtbl.set ctx ~key:"integer?"
    ~data:(mk_tp (function Integer _ -> true | _ -> false));
  Hashtbl.set signature ~key:"integer?" ~data:"'a -> bool";
  Hashtbl.set ctx ~key:"float?"
    ~data:(mk_tp (function Float _ -> true | _ -> false));
  Hashtbl.set signature ~key:"float?" ~data:"'a -> bool";
  Hashtbl.set ctx ~key:"symbol?"
    ~data:(mk_tp (function Symbol _ -> true | _ -> false));
  Hashtbl.set signature ~key:"symbol?" ~data:"'a -> bool";
  Hashtbl.set ctx ~key:"string?"
    ~data:(mk_tp (function String _ -> true | _ -> false));
  Hashtbl.set signature ~key:"string?" ~data:"'a -> bool";
  Hashtbl.set ctx ~key:"list?"
    ~data:(mk_tp (function List _ -> true | _ -> false));
  Hashtbl.set signature ~key:"list?" ~data:"'a -> bool";

  let nil_pred = function
    | [ List [] ] -> Bool true
    | [ List _ ] -> Bool false
    | _ -> "'nil?' expects an argument of type list" |> failwith
  in
  Hashtbl.set ctx ~key:"nil?" ~data:(Function nil_pred);
  Hashtbl.set signature ~key:"list?" ~data:"list 'a -> bool";

  let add_primitive name int_op float_op =
    let bin = function
      | [ Float a; Float b ] -> Float (float_op a b)
      | [ Integer a; Integer b ] -> Integer (int_op a b)
      | [ Integer a; Float b ] -> Float (float_op (Float.of_int a) b)
      | [ Float a; Integer b ] -> Float (float_op a (Float.of_int b))
      | _ -> Invalid_argument ("Invalid arguments to " ^ name) |> raise
    in
    Hashtbl.set ctx ~key:name ~data:(Function bin);
    Hashtbl.set signature ~key:name
      ~data:
        "integer -> integer -> integer\n\
         float -> float -> float\n\
         integer -> float -> float\n\
         float -> integer -> float"
  in
  let add_cmp name int_op float_op =
    let cmp = function
      | [ Float a; Float b ] -> Bool (float_op a b)
      | [ Integer a; Integer b ] -> Bool (int_op a b)
      | [ Integer a; Float b ] -> Bool (float_op (Float.of_int a) b)
      | [ Float a; Integer b ] -> Bool (float_op a (Float.of_int b))
      | _ -> Invalid_argument ("Invalid arguments to " ^ name) |> raise
    in
    Hashtbl.set ctx ~key:name ~data:(Function cmp);
    Hashtbl.set signature ~key:name
      ~data:
        "integer -> integer -> bool\n\
         float -> float -> bool\n\
         integer -> float -> bool\n\
         float -> integer -> bool"
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
    Hashtbl.set ctx ~key:name ~data:(Function cmp);
    Hashtbl.set signature ~key:name
      ~data:
        "integer -> integer -> bool\n\
         float -> float -> bool\n\
         integer -> float -> bool\n\
         float -> integer -> bool\n\
         bool -> bool -> bool"
  in

  add_cmp ">" Int.( > ) Float.( > );
  add_cmp "<" Int.( < ) Float.( < );
  add_cmp "<>" Int.( <> ) Float.( <> );
  add_cmp ">=" Int.( >= ) Float.( >= );
  add_cmp "<=" Int.( <= ) Float.( <= );
  add_primitive "+" ( + ) ( +. );
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
  (*Hashtbl.set signature ~key:"cons" ~data:("list 'a -> 'a");*)
  let car_op = function
    | [ List (x :: _) ] -> x
    | [ List [] ] -> "car being applied to an empty List" |> failwith
    | _ -> "Invalid argument to car" |> failwith
  in
  Hashtbl.set ctx ~key:"car" ~data:(Function car_op);
  Hashtbl.set signature ~key:"car" ~data:"list 'a -> 'a";
  let cdr_op = function
    | [ List (_ :: xs) ] -> List xs
    | [ List [] ] -> "cdr being applied to an empty List " |> failwith
    | _ -> "Invalid Argument to cdr" |> failwith
  in
  Hashtbl.set ctx ~key:"cdr" ~data:(Function cdr_op);
  Hashtbl.set signature ~key:"cdr" ~data:"list 'a -> 'a";
  let append_op = function
    | [ List y'; List x' ] -> List (List.append x' y')
    | [ y'; List x' ] -> List (x' @ [ y' ])
    | _ ->
        "append (&) expects two arguments (List, List) or (List, value)"
        |> failwith
  in
  Hashtbl.set ctx ~key:"append" ~data:(Function append_op);
  Hashtbl.set signature ~key:"append"
    ~data:"list 'a -> list 'a -> list 'a\n'a -> list 'a -> list 'a";
  Hashtbl.set ctx ~key:"&" ~data:(Function append_op);
  Hashtbl.set signature ~key:"&"
    ~data:"list 'a -> list 'a -> list 'a\n'a -> list 'a -> list 'a";

  let length_op = function
    | [ List x' ] -> Integer (List.length x')
    | _ -> "Length expects an argument of type List" |> failwith
  in
  Hashtbl.set ctx ~key:"length" ~data:(Function length_op);
  Hashtbl.set signature ~key:"length" ~data:"list 'a -> integer";

  let map_op = function
    | [ Function f; List x' ] -> List (List.map ~f:(fun x -> f [ x ]) x')
    | _ -> "Map expects 2 arguments of type (function λ, list)" |> failwith
  in
  Hashtbl.set ctx ~key:"map" ~data:(Function map_op);
  Hashtbl.set signature ~key:"map" ~data:"('a -> 'a) -> list 'a -> list 'a";

  let filter_op = function
    | [ Function f; List x' ] ->
        let truthy arg = match arg with Bool true -> true | _ -> false in
        List (List.filter ~f:(fun x -> truthy (f [ x ])) x')
    | _ -> "Filter expects 2 argumetns of type (function λ, list)" |> failwith
  in
  Hashtbl.set ctx ~key:"filter" ~data:(Function filter_op);
  Hashtbl.set signature ~key:"filer" ~data:"('a -> 'a) -> list 'a -> list 'a";

  let foldl_op = function
    | [ init; List x'; Function f ] ->
        List.fold_left x' ~init ~f:(fun acc x -> f [ acc; x ])
    | _ -> "Incorrect usage of foldl: foldl <acc> <list> <function>" |> failwith
  in
  Hashtbl.set ctx ~key:"foldl" ~data:(Function foldl_op);
  Hashtbl.set signature ~key:"foldl" ~data:"'a -> list 'a -> ('a -> 'a) -> 'a";

  let foldr_op = function
    | [ List x'; init; Function f ] ->
        List.fold_right x' ~init ~f:(fun acc x -> f [ x; acc ])
    | _ -> "Incorrect usage of foldr: foldl <list> <acc> <function>" |> failwith
  in
  Hashtbl.set ctx ~key:"foldr" ~data:(Function foldr_op);
  Hashtbl.set signature ~key:"foldr" ~data:"list 'a -> 'a -> ('a -> 'a) -> 'a";

  let sort_asc = function
    | [ List x' ] -> List (List.sort x' ~compare:Stdlib.compare)
    | _ ->
        "Incorrect usage of sort_asc: Only accepts arguments of type List"
        |> failwith
  in
  Hashtbl.set ctx ~key:"sort_asc" ~data:(Function sort_asc);
  Hashtbl.set signature ~key:"sort_asc" ~data:"list 'a -> list 'a";

  let sort_desc = function
    | [ List x' ] -> List (List.rev (List.sort x' ~compare:Stdlib.compare))
    | _ ->
        "Incorrect usage of sort_desc: Only accepts arguments of type List"
        |> failwith
  in
  Hashtbl.set ctx ~key:"sort_desc" ~data:(Function sort_desc);
  Hashtbl.set signature ~key:"sort_desc" ~data:"list 'a -> list 'a";

  let getn = function
    | [ Integer n; List x' ] -> (
        match List.nth x' n with
        | Some x -> x
        | None ->
            "Invalid n value '" ^ Stdlib.string_of_int n
            ^ "' for 'get n <list>'. Maximum bound for list is '"
            ^ Stdlib.string_of_int (List.length x' - 1)
            ^ "'"
            |> failwith)
    | _ -> "Invalid argument(s) to get n <list>" |> failwith
  in
  Hashtbl.set ctx ~key:"get" ~data:(Function getn);
  Hashtbl.set signature ~key:"get" ~data:"integer a -> list 'a -> 'a";

  let setn = function
    | [ Integer n; List x'; v ] when List.length x' > n ->
        let replace l pos a =
          List.mapi l ~f:(fun i x -> if i = pos then a else x)
        in
        List (replace x' n v)
    | [ Integer n; List x'; _ ] ->
        "Invalid n value '" ^ Stdlib.string_of_int n
        ^ "' for 'set n <list> <value>'. Maximum bound for list is '"
        ^ Stdlib.string_of_int (List.length x' - 1)
        ^ "'"
        |> failwith
    | _ -> "Invalid argument(s) to get n <list>" |> failwith
  in
  Hashtbl.set ctx ~key:"set" ~data:(Function setn);
  Hashtbl.set signature ~key:"set" ~data:"integer a -> list 'a -> 'a -> list 'a";

  let list_constructor args = List args in
  (* signature ? *)
  Hashtbl.set ctx ~key:"list" ~data:(Function list_constructor);
  Hashtbl.set signature ~key:"list" ~data:"list 'a -> list 'a";
  Hashtbl.set ctx ~key:"@" ~data:(Function list_constructor);
  Hashtbl.set signature ~key:"@" ~data:"list 'a -> list 'a";

  Hashtbl.set ctx ~key:"nil" ~data:(List []);
  Hashtbl.set signature ~key:"nil" ~data:"empty list 'a";
  (ctx, signature)

(* Evaluate a value in the given context *)
let rec eval ctx = function
  | Bool b -> Bool b
  | Integer n -> Integer n
  | Float n -> Float n
  | String s -> String s
  | Symbol s -> (
      match Hashtbl.find ctx s with
      | Some v -> v
      | None -> "Undefined symbol: " ^ s |> failwith)
  | List [] -> List []
  | List [ Symbol "if"; cond; then_expr; else_expr ] -> (
      match eval ctx cond with
      | Bool true -> eval ctx then_expr
      | Bool false -> eval ctx else_expr
      | _ -> "Invalid condition in if expression" |> failwith)
  | List [ Symbol "define"; Symbol name; expr ] ->
      let value = eval ctx expr in
      Hashtbl.set ctx ~key:name ~data:value;
      value
  | List [ Symbol "$"; List params; body ] ->
      let param_names =
        List.map
          ~f:(function
            | Symbol name -> name | _ -> "Invalid Parameter anme" |> failwith)
          params
      in
      Function
        (fun args ->
          if List.length args <> List.length param_names then
            "Argument count mismatch dumbass" |> failwith
          else
            let tmp_ctx = Hashtbl.copy ctx in
            List.iter2_exn param_names args ~f:(fun key value ->
                Hashtbl.set tmp_ctx ~key ~data:value);
            eval tmp_ctx body)
  | List (fn :: args) -> (
      let fn_val = eval ctx fn in
      let arg_vals = List.map ~f:(eval ctx) args in
      match fn_val with
      | Function f -> f arg_vals
      | _ -> "Not a function" |> failwith)
  | _ -> "Invalid expression" |> failwith

(* Parse and evaluate a string *)
let eval_string ctx str =
  let tokens = tokenize str in
  let s_exp = s_expression_of_token_list tokens in
  let values = of_sexp s_exp in
  eval ctx values

(* Pretty printer for Lisp values *)
let rec string_of_value = function
  | Integer n -> Int.to_string n
  | Float n -> Float.to_string n
  | Bool b -> begin match b with | true -> "real" | false -> "cap" end
  | String s -> "'" ^ s ^ "'"
  | Symbol s -> s
  (* implement cons dumbass *)
  | List vs ->
      "(" ^ String.concat ~sep:" " (List.map ~f:string_of_value vs) ^ ")"
  | Function _ -> "<function λ>"

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

        if !open_parens = !close_parens then Some (Buffer.contents buffer)
        else read_lines false
  in
  read_lines true

(* REPL main loop *)
let run_repl () =
  let env, signature = create_initial_env () in

  printf "Mini-Lisp REPL\n";
  printf "Type ':wq' to exit\n";
  printf "Type ':help' for commands\n\n";

  let rec loop () =
    match read_complete_expr () with
    | None -> printf "\nGoodbye!\n"
    | Some input -> (
        let trimmed = String.strip input in
        match trimmed with
        | "" -> loop ()
        | ":wq" -> printf ":wq!\n"
        | ":help" ->
            printf "\navailable commands:\n";
            printf "  :help  - show this help message\n";
            printf "  :wq - exit the repl\n";
            printf
              "  :env (optional enviroment variable)  - show current environment\n";
            printf
              "  :signature (optional variable)  - show current signature\n";
            printf "\nexample expressions:\n";
            printf "  (+ 1 2)\n";
            printf "  (define x 42)\n";
            printf "  (if (< 1 2) 10 20)\n";
            printf "  (define incr ($ (x) (+ x 1)))\n\n";
            loop ()
        | ":env" ->
            printf "\nCurrent environment:\n";
            Hashtbl.iteri env ~f:(fun ~key ~data ->
                printf "  %s: %s\n" key (string_of_value data));
            printf "\n";
            loop ()
        | ":signature" ->
            printf "\nSignatures:\n\n";
            Hashtbl.iteri signature ~f:(fun ~key ~data ->
                printf "\ntype signature(s) for '%s': \n%s\n" key data);
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
        | input
          when String.length input > 11
               && (String.equal (String.sub input ~pos:0 ~len:11)) ":signature "
          ->
            let key =
              String.sub input ~pos:11 ~len:(String.length input - 11)
            in
            (match Hashtbl.find signature key with
            | Some v -> printf "type signature(s) for '%s': \n%s\n" key v
            | None -> printf "'%s' not found in env\n" key);
            loop ()
        | _ -> (
            try
              let result = eval_string env trimmed in
              printf "=> %s\n" (string_of_value result);
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
