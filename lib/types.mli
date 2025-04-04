type value =
  | Integer of int
  | Float of float
  | Symbol of string
  | String of string
  | Bool of bool
  | List of value list
  | Function of (value list -> value)

type ctx = (string, value) Hashtbl.t
