val str : string list -> string

type token =
  | SymbolToken of string
  | ListOpening
  | ListClosing
  | EndOfTransmission

val rest : string -> string
val slurp_until_end_quote_and_give_me_the_string : string -> string
val slurp_until_symbol_end : string -> string
val tokenize : string -> token list

type s_expression =
  | Atom of string
  | SexpList of s_expression list

type ('i, 'e) parse_result =
  | ParseNext of 'i * 'e
  | ParseOut of 'i
  | ParseEnd

val parse_list
  :  'a
  -> ('a -> 'b list -> 'c * s_expression list)
  -> ('c, s_expression) parse_result

val parse_expression
  :  int * token list
  -> (int * token list -> 'a list -> (int * token list) * s_expression list)
  -> (int * token list, s_expression) parse_result

val string_of_s_expression : s_expression -> string
val s_expression_of_token_list : token list -> s_expression
