(* Copyright (c) 2025 Hubert Gruniaux
 *
 * This is free and unencumbered software released into the public domain.
 *
 * Anyone is free to copy, modify, publish, use, compile, sell, or
 * distribute this software, either in source code form or as a compiled
 * binary, for any purpose, commercial or non-commercial, and by any
 * means.
 *
 * In jurisdictions that recognize copyright laws, the author or authors
 * of this software dedicate any and all copyright interest in the
 * software to the public domain. We make this dedication for the benefit
 * of the public at large and to the detriment of our heirs and
 * successors. We intend this dedication to be an overt act of
 * relinquishment in perpetuity of all present and future rights to this
 * software under copyright law.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * For more information, please refer to <https://unlicense.org/>
 *)

open Tast

type t = ttype

(** Creates a new fresh type variable. *)
let fresh_var_raw =
  let counter = ref 0 in
  fun current_level ->
    let id = !counter in
    incr counter;
    { ttype_var_id = id; ttype_var_def = None; ttype_var_level = current_level }

let fresh_var current_level = Ttype_var (fresh_var_raw current_level)

let rec prune ?(keep_parens = false) = function
  | Ttype_var ({ ttype_var_def = Some t; _ } as tvar) ->
      (* Never keep parentheses after the first prune. *)
      let t' = prune ~keep_parens:false t in
      (* Path compression for faster subsequent lookups. *)
      tvar.ttype_var_def <- Some t';
      t'
  | Ttype_paren t when not keep_parens -> prune ~keep_parens t
  | t -> t

let rec equal t1 t2 =
  match (prune t1, prune t2) with
  | Ttype_bottom, Ttype_bottom -> true
  | Ttype_int, Ttype_int -> true
  | Ttype_float, Ttype_float -> true
  | Ttype_string, Ttype_string -> true
  | Ttype_char, Ttype_char -> true
  | Ttype_var v1, Ttype_var v2 -> v1.ttype_var_id = v2.ttype_var_id
  | Ttype_paren t1_inner, _ -> equal t1_inner t2
  | _, Ttype_paren t2_inner -> equal t1 t2_inner
  | Ttype_named (_, t1_inner), _ -> equal t1_inner t2
  | _, Ttype_named (_, t2_inner) -> equal t1 t2_inner
  | Ttype_enum enum1, Ttype_enum enum2 -> enum1 == enum2
  | Ttype_function (params1, ret1), Ttype_function (params2, ret2)
  | Ttype_node (params1, ret1), Ttype_node (params2, ret2) -> (
      try List.for_all2 equal params1 params2 && equal ret1 ret2
      with Invalid_argument _ -> false)
  | Ttype_tuple types1, Ttype_tuple types2 -> (
      try List.for_all2 equal types1 types2 with Invalid_argument _ -> false)
  | _ -> false

let rec compare t1 t2 =
  match (prune t1, prune t2) with
  | Ttype_bottom, Ttype_bottom -> 0
  | Ttype_bottom, _ -> -1
  | _, Ttype_bottom -> 1
  | Ttype_int, Ttype_int -> 0
  | Ttype_int, _ -> -1
  | _, Ttype_int -> 1
  | Ttype_float, Ttype_float -> 0
  | Ttype_float, _ -> -1
  | _, Ttype_float -> 1
  | Ttype_string, Ttype_string -> 0
  | Ttype_string, _ -> -1
  | _, Ttype_string -> 1
  | Ttype_char, Ttype_char -> 0
  | Ttype_char, _ -> -1
  | _, Ttype_char -> 1
  | Ttype_var v1, Ttype_var v2 -> Stdlib.compare v1.ttype_var_id v2.ttype_var_id
  | Ttype_var _, _ -> -1
  | _, Ttype_var _ -> 1
  | Ttype_paren t1_inner, _ -> compare t1_inner t2
  | _, Ttype_paren t2_inner -> compare t1 t2_inner
  | Ttype_named (_, t1_inner), _ -> compare t1_inner t2
  | _, Ttype_named (_, t2_inner) -> compare t1 t2_inner
  | Ttype_enum enum1, Ttype_enum enum2 ->
      Stdlib.compare (Obj.magic enum1) (Obj.magic enum2)
  | Ttype_enum _, _ -> -1
  | _, Ttype_enum _ -> 1
  | Ttype_function (params1, ret1), Ttype_function (params2, ret2)
  | Ttype_node (params1, ret1), Ttype_node (params2, ret2) ->
      let cmp_params = List.compare compare params1 params2 in
      if cmp_params <> 0 then cmp_params else compare ret1 ret2
  | Ttype_function _, _ -> -1
  | _, Ttype_function _ -> 1
  | Ttype_node _, _ -> -1
  | _, Ttype_node _ -> 1
  | Ttype_tuple types1, Ttype_tuple types2 -> List.compare compare types1 types2

(* --------------------------------------------------------
 * Unification
 *)

(** Checks that [tvar] does not occur in [t]. *)
let rec occurs tvar t =
  match prune t with
  | Ttype_bottom | Ttype_enum _ | Ttype_int | Ttype_float | Ttype_string
  | Ttype_char ->
      false
  | Ttype_var v -> v.ttype_var_id = tvar.ttype_var_id
  | Ttype_named (_, t) -> occurs tvar t
  | Ttype_paren t -> occurs tvar t
  | Ttype_function (params, ret) | Ttype_node (params, ret) ->
      List.exists (occurs tvar) params || occurs tvar ret
  | Ttype_tuple types -> List.exists (occurs tvar) types

(** Updates the levels of all free type variables in [tvar] to be at most
    [new_level]. *)
let rec update_levels tvar new_level =
  match prune tvar with
  | Ttype_var ({ ttype_var_def = None; _ } as v) ->
      if v.ttype_var_level > new_level then v.ttype_var_level <- new_level
  | Ttype_var { ttype_var_def = Some t; _ } -> update_levels t new_level
  | Ttype_named (_, t) -> update_levels t new_level
  | Ttype_paren t -> update_levels t new_level
  | Ttype_function (params, ret) | Ttype_node (params, ret) ->
      List.iter (fun t -> update_levels t new_level) params;
      update_levels ret new_level
  | Ttype_tuple types -> List.iter (fun t -> update_levels t new_level) types
  | Ttype_bottom | Ttype_enum _ | Ttype_int | Ttype_float | Ttype_string
  | Ttype_char ->
      ()

type unification_error =
  | OccursCheck of ttype_var * t
  | Mismatch of t * t
  | TooFewArguments of t * int * int
  | TooManyArguments of t * int * int

exception UnificationError of unification_error

let rec unify ?(read_only = false) (t1 : ttype) (t2 : ttype) : unit =
  let t1 = prune t1 and t2 = prune t2 in
  match (t1, t2) with
  (* everyting unify with bottom *)
  | Ttype_bottom, _ -> ()
  | _, Ttype_bottom -> ()
  (* unify type variables *)
  | Ttype_var v1, Ttype_var v2 when v1.ttype_var_id = v2.ttype_var_id -> ()
  | Ttype_var v1, _ ->
      if occurs v1 t2 then raise (UnificationError (OccursCheck (v1, t2)));
      if not read_only then begin
        update_levels t2 v1.ttype_var_level;
        v1.ttype_var_def <- Some t2
      end
  | _, Ttype_var v2 ->
      if occurs v2 t1 then raise (UnificationError (OccursCheck (v2, t1)));
      if not read_only then begin
        update_levels t1 v2.ttype_var_level;
        v2.ttype_var_def <- Some t1
      end
  (* ignore parentheses and name metadata *)
  | Ttype_named (_, t1), _ -> unify ~read_only t1 t2
  | _, Ttype_named (_, t2) -> unify ~read_only t1 t2
  | Ttype_paren t1, _ -> unify ~read_only t1 t2
  | _, Ttype_paren t2 -> unify ~read_only t1 t2
  | Ttype_int, Ttype_int -> ()
  | Ttype_float, Ttype_float -> ()
  | Ttype_string, Ttype_string -> ()
  | Ttype_char, Ttype_char -> ()
  | Ttype_enum enum1, Ttype_enum enum2 ->
      if enum1 != enum2 then raise (UnificationError (Mismatch (t1, t2)))
  | Ttype_function (params1, ret1), Ttype_function (params2, ret2)
  | Ttype_node (params1, ret1), Ttype_node (params2, ret2) -> (
      try
        List.iter2 (unify ~read_only) params1 params2;
        unify ~read_only ret1 ret2
      with Invalid_argument _ ->
        let len1 = List.length params1 in
        let len2 = List.length params2 in
        if len1 > len2 then
          raise (UnificationError (TooFewArguments (t1, len1, len2)))
        else if len1 < len2 then
          raise (UnificationError (TooManyArguments (t1, len1, len2)))
        else assert false)
  | Ttype_tuple types1, Ttype_tuple types2 -> (
      try List.iter2 (unify ~read_only) types1 types2
      with Invalid_argument _ -> raise (UnificationError (Mismatch (t1, t2))))
  | _ -> raise (UnificationError (Mismatch (t1, t2)))

(* --------------------------------------------------------
 * Generalization
 *)

let gvar_level = -1

let generalize level t =
  let gvars = ref [] in
  let rec aux t =
    match prune t with
    | Ttype_named (_, t) -> aux t
    | Ttype_paren t -> aux t
    | Ttype_var ({ ttype_var_def = None; _ } as tvar)
      when level < tvar.ttype_var_level ->
        tvar.ttype_var_level <- gvar_level;
        gvars := tvar :: !gvars
    | Ttype_var _ -> ()
    | Ttype_bottom | Ttype_int | Ttype_float | Ttype_string | Ttype_char
    | Ttype_enum _ ->
        ()
    | Ttype_function (params, ret) | Ttype_node (params, ret) ->
        List.iter aux params;
        aux ret
    | Ttype_tuple types -> List.iter aux types
  in
  aux t;
  !gvars

let instantiate level t =
  let subst_table = Hashtbl.create 7 in
  let rec aux t =
    match prune t with
    | Ttype_bottom | Ttype_int | Ttype_float | Ttype_string | Ttype_char
    | Ttype_enum _ ->
        t
    | Ttype_named (name, t) -> Ttype_named (name, aux t)
    | Ttype_paren t -> Ttype_paren (aux t)
    | Ttype_function (params, ret) ->
        Ttype_function (List.map aux params, aux ret)
    | Ttype_node (params, ret) -> Ttype_node (List.map aux params, aux ret)
    | Ttype_tuple types -> Ttype_tuple (List.map aux types)
    | Ttype_var ({ ttype_var_level = var_level; _ } as tvar)
      when var_level = gvar_level -> (
        (* This is a generalized variable, instantiate it *)
        match Hashtbl.find_opt subst_table tvar with
        | Some tvar -> tvar
        | None ->
            let new_var = fresh_var_raw level in
            Hashtbl.add subst_table tvar (Ttype_var new_var);
            Ttype_var new_var)
    | Ttype_var _ -> t
  in
  (aux t, subst_table)

(** [is_generalized v] returns true if the type variable [v] is generalized. The
    return list may have duplicates. *)
let is_generalized v = v.ttype_var_level = gvar_level

let generalized_variables t =
  let gvars = ref [] in
  let rec aux t =
    match prune t with
    | Ttype_named (_, t) -> aux t
    | Ttype_paren t -> aux t
    | Ttype_var ({ ttype_var_def = None; _ } as tvar) when is_generalized tvar
      ->
        gvars := tvar :: !gvars
    | Ttype_var _ -> ()
    | Ttype_bottom | Ttype_int | Ttype_float | Ttype_string | Ttype_char
    | Ttype_enum _ ->
        ()
    | Ttype_function (params, ret) | Ttype_node (params, ret) ->
        List.iter aux params;
        aux ret
    | Ttype_tuple types -> List.iter aux types
  in
  aux t;
  !gvars

(** [free_variables t] returns the list of free variables (not generalized and
    not yet unified) in the type [t]. The return list may have duplicates. *)
let free_variables t =
  let fvars = ref [] in
  let rec aux t =
    match prune t with
    | Ttype_named (_, t) -> aux t
    | Ttype_paren t -> aux t
    | Ttype_var ({ ttype_var_def = None; _ } as tvar)
      when not (is_generalized tvar) ->
        fvars := tvar :: !fvars
    | Ttype_var _ -> ()
    | Ttype_bottom | Ttype_int | Ttype_float | Ttype_string | Ttype_char
    | Ttype_enum _ ->
        ()
    | Ttype_function (params, ret) | Ttype_node (params, ret) ->
        List.iter aux params;
        aux ret
    | Ttype_tuple types -> List.iter aux types
  in
  aux t;
  !fvars

(** [is_node t] returns true if the type [t] is a node type. *)
let is_node t = match prune t with Ttype_node _ -> true | _ -> false

(* --------------------------------------------------------
 * Builtin bool type
 *)

(** The builtin bool type. *)
let bool_declaration, false_constructor, true_constructor =
  let create_bool_type () =
    let enum_decl =
      {
        tdecl_enum_name = { value = "bool"; loc = Location.dummy };
        tdecl_enum_constructors = [];
        tdecl_enum_loc = Location.dummy;
      }
    in

    let create_constructor name =
      {
        tdecl_constructor_name = { value = name; loc = Location.dummy };
        tdecl_constructor_enum = enum_decl;
        tdecl_constructor_loc = Location.dummy;
      }
    in

    let false_constructor = create_constructor "false" in
    let true_constructor = create_constructor "true" in
    enum_decl.tdecl_enum_constructors <- [ false_constructor; true_constructor ];
    (enum_decl, false_constructor, true_constructor)
  in
  create_bool_type ()

let bool_type = Ttype_enum bool_declaration

(* --------------------------------------------------------
 * Builtin unit type
 *)

(** The builtin unit type. *)
let unit_declaration, unit_constructor =
  let enum_decl =
    {
      tdecl_enum_name = { value = "unit"; loc = Location.dummy };
      tdecl_enum_constructors = [];
      tdecl_enum_loc = Location.dummy;
    }
  in

  (* The unit constructor is quite special! It is handled specially by the
     parser as its name is not even an identifier. Moreover, it is not
     added to the global declaration table. *)
  let constructor =
    {
      tdecl_constructor_name = { value = "()"; loc = Location.dummy };
      tdecl_constructor_enum = enum_decl;
      tdecl_constructor_loc = Location.dummy;
    }
  in
  enum_decl.tdecl_enum_constructors <- [ constructor ];
  (enum_decl, constructor)

let unit_type = Ttype_enum unit_declaration

let is_unit t =
  match prune t with Ttype_enum decl -> decl == unit_declaration | _ -> false

(* --------------------------------------------------------
 * Bool-like types utilities
 *)

let rec is_bool_like t =
  match prune t with
  | Ttype_enum decl -> (
      (* Has exactly two constructors. *)
      match decl.tdecl_enum_constructors with
      | [ _; _ ] -> true
      | _ -> false)
  | Ttype_bottom -> true
  | Ttype_paren t_inner -> is_bool_like t_inner
  | Ttype_named (_, t_inner) -> is_bool_like t_inner
  | _ -> false

let truthy_constructor_of t =
  assert (is_bool_like t);
  match prune t with
  | Ttype_enum decl -> List.hd (List.tl decl.tdecl_enum_constructors)
  | Ttype_bottom -> true_constructor
  | _ -> assert false

let falsy_constructor_of t =
  assert (is_bool_like t);
  match prune t with
  | Ttype_enum decl -> List.hd decl.tdecl_enum_constructors
  | Ttype_bottom -> false_constructor
  | _ -> assert false
