open Base

type ty =
  | TVar of int (* Type variables for inference *)
  | TInt (* Integer type *)
  | TFloat (* Float type *)
  | TBool (* Boolean type *)
  | TString (* String type *)
  | TList of ty (* List type *)
  | TFun of ty list * ty (* Function type: args * return *)
  | TPoly of string * ty (* Polymorphic type *)
  | TUserDefined of string * ty list (* User-defined type with parameters *)

type schema =
  { vars : string list
  ; ty : ty
  }

type typed_value =
  { value : Types.value
  ; ty : ty
  }

type type_ctx = (string * schema) list

let next_type_var =
  let counter = ref 0 in
  fun () ->
    Int.incr counter;
    TVar !counter
;;

(* Fresh type variable name generator *)
let next_type_name =
  let counter = ref 0 in
  fun () ->
    Int.incr counter;
    "'t" ^ Int.to_string !counter
;;

module IntSet = Stdlib.Set.Make (Int)
module StringSet = Stdlib.Set.Make (String)

let rec free_vars = function
  | TVar n -> IntSet.singleton n
  | TInt | TFloat | TBool | TString -> IntSet.empty
  | TList t -> free_vars t
  | TFun (args, ret) ->
    let init' = free_vars ret in
    let f' acc arg = IntSet.union acc (free_vars arg) in
    List.fold args ~init:init' ~f:f'
  | TPoly (_, t) -> free_vars t
  | TUserDefined (_, args) ->
    List.fold args ~init:IntSet.empty ~f:(fun acc arg -> IntSet.union acc (free_vars arg))
;;

type subst = (int * ty) list

let rec apply_subst subst t =
  match t with
  | TVar n ->
    (match List.Assoc.find subst ~equal:Int.equal n with
     | Some t' -> t'
     | None -> t)
  | TList t' -> TList (apply_subst subst t')
  | TFun (args, ret) -> TFun (List.map args ~f:(apply_subst subst), apply_subst subst ret)
  | TPoly (name, t') -> TPoly (name, apply_subst subst t')
  | TUserDefined (name, args) -> TUserDefined (name, List.map args ~f:(apply_subst subst))
  | t -> t
;;

let compose_subst (s1 : subst) (s2 : subst) : subst =
  let apply_s1 = List.map s2 ~f:(fun (n, t) -> n, apply_subst s1 t) in
  s1 @ apply_s1
;;

let rec occurs_check (n : int) (t : ty) : bool =
  match t with
  | TVar m -> Int.equal n m
  | TList t' -> occurs_check n t'
  | TFun (args, ret) -> List.exists args ~f:(occurs_check n) || occurs_check n ret
  | TPoly (_, t') -> occurs_check n t'
  | TUserDefined (_, args) -> List.exists args ~f:(occurs_check n)
  | _ -> false
;;

(* Unification with polymorphic types *)
let rec unify (t1 : ty) (t2 : ty) : subst =
  match t1, t2 with
  | t1, t2 when phys_equal t1 t2 -> []
  | TVar n, t | t, TVar n ->
    if occurs_check n t then failwith "Occurs check failed" else [ n, t ]
  | TList t1', TList t2' -> unify t1' t2'
  | TFun (args1, ret1), TFun (args2, ret2) ->
    if List.length args1 <> List.length args2
    then failwith "Function argument count mismatch"
    else (
      let s1 =
        List.fold2_exn args1 args2 ~init:[] ~f:(fun s t1 t2 ->
          compose_subst s (unify (apply_subst s t1) (apply_subst s t2)))
      in
      let s2 = unify (apply_subst s1 ret1) (apply_subst s1 ret2) in
      compose_subst s1 s2)
  | TPoly (n1, t1), TPoly (n2, t2) when String.equal n1 n2 -> unify t1 t2
  | TUserDefined (n1, args1), TUserDefined (n2, args2) when String.equal n1 n2 ->
    if List.length args1 <> List.length args2
    then failwith "Type argument count mismatch"
    else
      List.fold2_exn args1 args2 ~init:[] ~f:(fun s t1 t2 ->
        compose_subst s (unify (apply_subst s t1) (apply_subst s t2)))
  | _, _ -> failwith "Type mismatch"
;;

let generalize (env : type_ctx) (ty : ty) : schema =
  let env_tvars =
    List.fold env ~init:IntSet.empty ~f:(fun acc (_, scheme) ->
      IntSet.union acc (free_vars scheme.ty))
  in
  let free = IntSet.diff (free_vars ty) env_tvars in
  let vars = IntSet.to_list free |> List.map ~f:(fun _ -> next_type_name ()) in
  let subst =
    List.zip_exn (IntSet.to_list free) (List.map vars ~f:(fun v -> TPoly (v, TVar 0)))
  in
  { vars; ty = apply_subst subst ty }
;;
