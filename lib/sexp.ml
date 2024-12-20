open Base
open Parsexp
open Stdio

type value =
  | Number of int
  | Float of float
  | Symbol of string
  | Bool of bool
  | List of value list
  | Function of (value list -> value)

(* Environment to store variables and functions *)
type ctx = (string, value) Hashtbl.t


(* Convert Sexp.t to value *)
let rec of_sexp = function
  | Sexp.Atom s -> begin
      match Int.of_string_opt s with
       | Some n -> Number n
       | None -> begin
           match Float.of_string_opt s with
           | Some n -> Float n
           | None -> Symbol s
           end
    end
 | Sexp.List xs -> List (List.map ~f:of_sexp xs)

(* Create initial environment with basic operations *)
let create_initial_env () =
  let ctx = Hashtbl.create (module String) in
      let add_primitive name int_op float_op =
      let bin = function
         | [ Float a; Float b ]   -> Float (float_op a b)
         | [ Number a; Number b ] -> Number (int_op a b)
         | [ Number a; Float b ]  -> Float (float_op (Float.of_int a) b)
         | [ Float a; Number b ]  -> Float(float_op a (Float.of_int b))
         | _ -> raise (Invalid_argument ("Invalid arguments to " ^ name));
      in
    Hashtbl.set ctx ~key:name ~data:(Function bin);
  in
  let add_cmp name int_op float_op =
      let cmp = function
         | [ Float a; Float b ] -> Bool (float_op a b)
         | [ Number a; Number b ] -> Bool (int_op a b)
         | [ Number a; Float b ] -> Bool (float_op (Float.of_int a) b)
         | [ Float a; Number b ] -> Bool (float_op a (Float.of_int b))
         | _ -> raise (Invalid_argument ("Invalid arguments to " ^ name));
      in
    Hashtbl.set ctx ~key:name ~data:(Function cmp)
  in
  add_cmp ("=") ( Int.equal ) ( Float.equal );
  add_cmp (">") ( Int.(>) ) ( Float.(>) );
  add_cmp ("<") ( Int.(<) ) ( Float.(<) );
  add_cmp ("<>") ( Int.(<>) ) ( Float.(<>) );
  add_cmp (">=") ( Int.(>=) ) ( Float.(>=) );
  add_cmp ("<=") ( Int.(<=) ) ( Float.(<=) );
  add_primitive ("+") ( + ) ( +. );
  add_primitive ("*") ( * ) ( *. );
  add_primitive ("/") ( / ) ( /. );
  add_primitive ("-") ( - ) ( -. );
  add_primitive ("^") ( ** ) ( **. );
  ctx



(* Evaluate a value in the given context *)
let rec eval ctx = function
  | Number n -> Number n
  | Float n -> Float n
  | Symbol s -> (
      match Hashtbl.find ctx s with
      | Some v -> v
      | None -> failwith ("Undefined symbol: " ^ s))
  | List [] -> List []
  | List [ Symbol "if"; cond; then_expr; else_expr ] -> (
      match eval ctx cond with
      | Bool true -> eval ctx then_expr
      | Bool false -> eval ctx else_expr
      | _ -> failwith "Invalid condition in if expression")
  | List [ Symbol "define"; Symbol name; expr ] ->
      let value = eval ctx expr in
      Hashtbl.set ctx ~key:name ~data:value;
      value
  | List [ Symbol "lambda"; List params; body ] ->
      let param_names =
        List.map
          ~f:(function
            | Symbol name -> name | _ -> failwith "Invalid Parameter anme")
          params
      in
      Function
        (fun args ->
          if List.length args <> List.length param_names then
            failwith "Argument count mismatch dumbass"
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
      | _ -> failwith "Not a function")
  | _ -> failwith "Invalid expression"

(* Parse and evaluate a string *)
let eval_string ctx str =
  match Single.parse_string str with
  | Ok sexp -> eval ctx (of_sexp sexp)
  | Error err ->
      failwith
        (String.concat ~sep:": "
           [ "Parse error"; Parsexp.Parse_error.message err ])


(* Pretty printer for Lisp values *)
let rec string_of_value = function
  | Number n -> Int.to_string n
  | Float n -> Float.to_string n
  | Bool b -> Bool.to_string b
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
    printf "%s" (if first_line then "λ> " else "... > ");
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

let help = printf "\nAvailable commands:\n";
            printf "  :help  - Show this help message\n";
            printf "  :quit  - Exit the REPL\n";
            printf "  :env   - Show current environment\n";
            printf "\nExample expressions:\n";
            printf "  (+ 1 2)\n";
            printf "  (define x 42)\n";
            printf "  (if (< 1 2) 10 20)\n\n";

(* REPL main loop *)
let run_repl () =
  let env = create_initial_env () in

  printf "Mini-Lisp REPL\n";
  printf "Type ':quit' to exit\n";
  printf "Type ':help' for commands\n\n";

  let rec loop () =
    match read_complete_expr () with
    | None -> printf "\nGoodbye!\n"
    | Some input -> (
        let trimmed = String.strip input in
        match trimmed with
        | "" -> loop ()
        | ":quit" -> printf ":wq!\n"
        | ":help" -> help(); loop ()
        | ":env" ->
            printf "\nCurrent environment:\n";
            Hashtbl.iteri env ~f:(fun ~key ~data ->
                printf "  %s: %s\n" key (string_of_value data));
            printf "\n";
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

