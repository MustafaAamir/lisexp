open Types

val of_sexp : Parser.s_expression -> value
val create_initial_env : unit -> (string, value) Base.Hashtbl.t
val eval : (string, value) Base.Hashtbl.t -> value -> value
val eval_string : (string, value) Base.Hashtbl.t -> string -> value
val string_of_value : value -> string
val read_complete_expr : unit -> string option
val help_message : string
val run_repl : unit -> unit
