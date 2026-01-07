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

type t = tclock

(** Creates a new fresh clock variable. *)
let fresh_var_raw =
  let counter = ref 0 in
  fun current_level ->
    let id = !counter in
    incr counter;
    {
      tclock_var_id = id;
      tclock_var_def = None;
      tclock_var_level = current_level;
    }

(** [fresh_var level] creates a fresh clock variable with the given [level]. *)
let fresh_var current_level = Tclock_var (fresh_var_raw current_level)

(** [prune ?keep_parens c] returns the representative of the clock [c],
    following any clock variable bindings. If [keep_parens] is true,
    parenthesesed clocks are preserved. *)
let rec prune ?(keep_parens = false) c =
  match c with
  | Tclock_var ({ tclock_var_def = Some c; _ } as v) ->
      (* Never keep parentheses after the first prune. *)
      let c' = prune ~keep_parens:false c in
      (* Path compression for faster subsequent lookups. *)
      v.tclock_var_def <- Some c';
      c'
  | Tclock_paren c_inner when not keep_parens -> prune ~keep_parens c_inner
  | _ -> c

let%test "clock prune test" =
  let v1 = fresh_var_raw 0 in
  let v2 = fresh_var_raw 0 in
  let c1 = Tclock_var v1 in
  let c2 = Tclock_var v2 in
  v1.tclock_var_def <- Some c2;
  v2.tclock_var_def <- Some Tclock_static;
  match prune c1 with Tclock_static -> true | _ -> false

let%test "clock prune with keeping parens test" =
  let v1 = fresh_var_raw 0 in
  let v2 = fresh_var_raw 0 in
  let c1 = Tclock_paren (Tclock_var v1) in
  let c2 = Tclock_var v2 in
  v1.tclock_var_def <- Some c2;
  v2.tclock_var_def <- Some Tclock_static;
  match prune ~keep_parens:true c1 with
  | Tclock_paren (Tclock_var v) when v.tclock_var_id = v1.tclock_var_id -> true
  | _ -> false

let%test "clock prune without keeping parens test" =
  let v1 = fresh_var_raw 0 in
  let v2 = fresh_var_raw 0 in
  let c1 = Tclock_paren (Tclock_var v1) in
  let c2 = Tclock_var v2 in
  v1.tclock_var_def <- Some c2;
  v2.tclock_var_def <- Some Tclock_static;
  match prune ~keep_parens:false c1 with Tclock_static -> true | _ -> false

(* --------------------------------------------------------
 * Equality
 *)

let mk_not_expr e =
  {
    texpr_type = e.texpr_type;
    texpr_clock = Tast.Tclock_static;
    texpr_kind = Texpr_unary ({ value = Ast.Aunop_not; loc = Location.dummy }, e);
    texpr_loc = e.texpr_loc;
  }

let simplify_dne_expr e =
  match e.texpr_kind with
  | Texpr_unary ({ value = Ast.Aunop_not; _ }, e_inner) -> (
      match e_inner.texpr_kind with
      | Texpr_unary ({ value = Ast.Aunop_not; _ }, e_double_inner) ->
          e_double_inner
      | _ -> e)
  | _ -> e

let equal_bool_when c1_ctor c1_cond c2_ctor c2_cond =
  if c1_ctor == c2_ctor then Expression.equal c1_cond c2_cond
  else
    Expression.equal c1_cond (simplify_dne_expr (mk_not_expr c2_cond))
    || Expression.equal (simplify_dne_expr (mk_not_expr c1_cond)) c2_cond

let is_boolean_ctor ctor = ctor.tdecl_constructor_enum == Type.bool_declaration

(** [equal c1 c2] checks if the two clocks [c1] and [c2] are equivalent
    (represent the same clock). *)
let rec equal c1 c2 =
  match (prune c1, prune c2) with
  | Tclock_static, Tclock_static -> true
  | Tclock_var v1, Tclock_var v2 -> v1.tclock_var_id = v2.tclock_var_id
  | Tclock_paren c1_inner, _ -> equal c1_inner c2
  | _, Tclock_paren c2_inner -> equal c1 c2_inner
  | Tclock_named (_, c1_inner), _ -> equal c1_inner c2
  | _, Tclock_named (_, c2_inner) -> equal c1 c2_inner
  | Tclock_tuple clocks1, Tclock_tuple clocks2 -> (
      try List.for_all2 equal clocks1 clocks2 with Invalid_argument _ -> false)
  | Tclock_function (params1, return1), Tclock_function (params2, return2) -> (
      try List.for_all2 equal params1 params2 && equal return1 return2
      with Invalid_argument _ -> false)
  | ( Tclock_on (c1_inner, c1_ctor, c1_cond),
      Tclock_on (c2_inner, c2_ctor, c2_cond) )
    when is_boolean_ctor c1_ctor && is_boolean_ctor c2_ctor ->
      (* Special case for booleans because we want that
           α when e ≡ α when not (not e) be true *)
      equal c1_inner c2_inner && equal_bool_when c1_ctor c1_cond c2_ctor c2_cond
  | ( Tclock_on (c1_inner, c1_ctor, c1_cond),
      Tclock_on (c2_inner, c2_ctor, c2_cond) ) ->
      equal c1_inner c2_inner && c1_ctor == c2_ctor
      && Expression.equal c1_cond c2_cond
  | _ -> false

let%test_unit "clock equality test" =
  let v1 = fresh_var_raw 0 in
  let v2 = fresh_var_raw 0 in
  let c1 = Tclock_var v1 in
  let c2 = Tclock_var v2 in
  assert (equal c1 c1);
  assert (not (equal c1 c2));

  assert (equal (Tclock_paren c1) c1);
  assert (not (equal (Tclock_paren c1) c2));

  let name = { Ast.value = "name"; loc = Location.dummy } in
  assert (equal (Tclock_named (name, c1)) c1);
  assert (not (equal (Tclock_named (name, c1)) c2));

  let tuple_c1 = Tclock_tuple [ c1; Tclock_static ] in
  let tuple_c2 = Tclock_tuple [ c2; Tclock_static ] in
  assert (equal tuple_c1 tuple_c1);
  assert (not (equal tuple_c1 tuple_c2));

  let func_c1 = Tclock_function ([ c1; Tclock_static ], c2) in
  let func_c2 = Tclock_function ([ c2; Tclock_static ], c2) in
  assert (equal func_c1 func_c1);
  assert (not (equal func_c1 func_c2))

(* --------------------------------------------------------
 * Unification
 *)

(** Checks that [cvar] does not occur in [c]. *)
let rec occurs cvar c =
  match prune c with
  | Tclock_static -> false
  | Tclock_var v -> v.tclock_var_id = cvar.tclock_var_id
  | Tclock_tuple clocks -> List.exists (fun clock -> occurs cvar clock) clocks
  | Tclock_paren c_inner -> occurs cvar c_inner
  | Tclock_named (_, c_inner) -> occurs cvar c_inner
  | Tclock_on (c_inner, _, _) -> occurs cvar c_inner
  | Tclock_function (params, return) ->
      List.exists (fun clock -> occurs cvar clock) params || occurs cvar return

let%test_unit "clock occurs test" =
  let v1 = fresh_var_raw 0 in
  let v2 = fresh_var_raw 0 in
  let c1 = Tclock_var v1 in
  let c2 = Tclock_var v2 in
  assert (occurs v1 c1);
  assert (not (occurs v1 c2));

  assert (occurs v1 (Tclock_paren c1));
  assert (not (occurs v1 (Tclock_paren c2)));

  let name = { Ast.value = "name"; loc = Location.dummy } in
  assert (occurs v1 (Tclock_named (name, c1)));
  assert (not (occurs v1 (Tclock_named (name, c2))));

  let tuple_c1 = Tclock_tuple [ c1; Tclock_static ] in
  let tuple_c2 = Tclock_tuple [ c2; Tclock_static ] in
  assert (occurs v1 tuple_c1);
  assert (not (occurs v1 tuple_c2));

  let func_c1 = Tclock_function ([ c1; Tclock_static ], c2) in
  let func_c2 = Tclock_function ([ c2; Tclock_static ], c2) in
  assert (occurs v1 func_c1);
  assert (not (occurs v1 func_c2))

(** Updates the levels of all free clock variables in [cvar] to be at most
    [new_level]. *)
let rec update_levels cvar new_level =
  match prune cvar with
  | Tclock_var ({ tclock_var_def = None; _ } as v) ->
      if v.tclock_var_level > new_level then v.tclock_var_level <- new_level
  | Tclock_var { tclock_var_def = Some v; _ } -> update_levels v new_level
  | Tclock_static -> ()
  | Tclock_named (_, c_inner) -> update_levels c_inner new_level
  | Tclock_paren c_inner -> update_levels c_inner new_level
  | Tclock_tuple clocks ->
      List.iter (fun clock -> update_levels clock new_level) clocks
  | Tclock_on (c_inner, _, _) -> update_levels c_inner new_level
  | Tclock_function (params, return) ->
      List.iter (fun clock -> update_levels clock new_level) params;
      update_levels return new_level

type unification_error =
  | OccursCheck of Tast.tclock_var * t
  | Mismatch of t * t
  | WhenConstructorMismatch of
      Tast.constructor_declaration * Tast.constructor_declaration
  | WhenConditionMismatch of Tast.texpression * Tast.texpression

exception UnificationError of unification_error

(** [unify c1 c2] unifies the two clocks [c1] and [c2], making them equivalent.
    Raises [UnificationError] if the clocks cannot be unified. *)
let rec unify c1 c2 =
  match (prune c1, prune c2) with
  (* static is compatible with any clock *)
  | Tclock_static, _ -> ()
  | _, Tclock_static -> ()
  | c1, c2 when equal c1 c2 -> ()
  (* unify clock variables *)
  | Tclock_var v1, Tclock_var v2 when v1.tclock_var_id = v2.tclock_var_id -> ()
  | Tclock_var v, _ ->
      if occurs v c2 then raise (UnificationError (OccursCheck (v, c2)));
      update_levels c2 v.tclock_var_level;
      v.tclock_var_def <- Some c2
  | _, Tclock_var v ->
      if occurs v c1 then raise (UnificationError (OccursCheck (v, c1)));
      update_levels c1 v.tclock_var_level;
      v.tclock_var_def <- Some c1
  (* ignore parentheses and name metadata *)
  | Tclock_paren c1_inner, _ -> unify c1_inner c2
  | _, Tclock_paren c2_inner -> unify c1 c2_inner
  | Tclock_named (_, c1_inner), _ -> unify c1_inner c2
  | _, Tclock_named (_, c2_inner) -> unify c1 c2_inner
  | Tclock_tuple clocks1, Tclock_tuple clocks2 -> (
      try List.iter2 unify clocks1 clocks2
      with Invalid_argument _ -> raise (UnificationError (Mismatch (c1, c2))))
  | Tclock_function (params1, return1), Tclock_function (params2, return2) -> (
      try
        List.iter2 unify params1 params2;
        unify return1 return2
      with Invalid_argument _ -> raise (UnificationError (Mismatch (c1, c2))))
  | ( Tclock_on (c1_inner, c1_ctor, c1_expr),
      Tclock_on (c2_inner, c2_ctor, c2_expr) ) ->
      if c1_ctor != c2_ctor then
        raise (UnificationError (WhenConstructorMismatch (c1_ctor, c2_ctor)))
      else if not (Expression.equal c1_expr c2_expr) then
        raise (UnificationError (WhenConditionMismatch (c1_expr, c2_expr)));
      unify c1_inner c2_inner
  | _ -> raise (UnificationError (Mismatch (c1, c2)))

let%test_unit "clock unify var with itself" =
  let v1 = fresh_var_raw 0 in
  unify (Tclock_var v1) (Tclock_var v1)

(* --------------------------------------------------------
 * Generalization
 *)

let gvar_level = -1

(** [generalize level c] generalizes all clock variables in [c] that have a
    level greater than [level], setting their level to [gvar_level] and
    returning the list of generalized clock variables. *)
let generalize level c =
  let gvars = ref [] in
  let rec aux c =
    match prune c with
    | Tclock_named (_, c) -> aux c
    | Tclock_paren c -> aux c
    | Tclock_var ({ tclock_var_def = None; _ } as cvar)
      when level < cvar.tclock_var_level ->
        cvar.tclock_var_level <- gvar_level;
        gvars := cvar :: !gvars
    | Tclock_var _ -> ()
    | Tclock_static -> ()
    | Tclock_tuple clocks -> List.iter aux clocks
    | Tclock_on (c, _, _expr) ->
        (* TODO: Correctly handle Tclock_on *)
        aux c
    | Tclock_function (params, return) ->
        List.iter aux params;
        aux return
  in
  aux c;
  !gvars

let%test_unit "generalize test" =
  let v1 = fresh_var_raw 1 in
  let v2 = fresh_var_raw 2 in
  let c =
    Tclock_tuple [ Tclock_var v1; Tclock_paren (Tclock_var v2); Tclock_static ]
  in
  let gvars = generalize 1 c in
  assert (List.length gvars = 1);
  assert (List.hd gvars == v2);
  assert (v1.tclock_var_level = 1);
  assert (v2.tclock_var_level = gvar_level)

(** [instantiate level clock] instantiates all clock variables in [clock] that
    have a level greater than [level], replacing them with fresh clock
    variables. *)
let instantiate level c =
  let subst_table = Hashtbl.create 7 in
  let rec aux c =
    match prune c with
    | Tclock_static -> c
    | Tclock_named (name, c) -> Tclock_named (name, aux c)
    | Tclock_paren c -> Tclock_paren (aux c)
    | Tclock_tuple clocks -> Tclock_tuple (List.map aux clocks)
    | Tclock_on (c, ctor, expr) ->
        (* TODO: Correctly handle Tclock_on *)
        Tclock_on (aux c, ctor, expr)
    | Tclock_function (params, return) ->
        Tclock_function (List.map aux params, aux return)
    | Tclock_var { tclock_var_level = var_level; tclock_var_id = var_id; _ }
      when var_level = gvar_level -> (
        (* This is a generalized variable, instantiate it *)
        match Hashtbl.find_opt subst_table var_id with
        | Some cvar -> Tclock_var cvar
        | None ->
            let new_var = fresh_var_raw level in
            Hashtbl.add subst_table var_id new_var;
            Tclock_var new_var)
    | Tclock_var _ -> c
  in
  aux c

let%test_unit "instantiate test" =
  let v1 = fresh_var_raw 0 in
  let v2 = fresh_var_raw 0 in
  v1.tclock_var_level <- gvar_level;
  let c = Tclock_tuple [ Tclock_var v1; Tclock_var v2 ] in
  let c' = instantiate 1 c in
  match c' with
  | Tclock_tuple [ Tclock_var v1'; Tclock_var v2' ] ->
      assert (v1'.tclock_var_id <> v1.tclock_var_id);
      assert (v1'.tclock_var_level = 1);
      assert (v2'.tclock_var_id = v2.tclock_var_id)
  | _ -> assert false

(** [has_on_clock clock] returns true if the given [clock] contains an 'on'
    clock. *)
let rec has_on_clock c =
  match prune c with
  | Tclock_on _ -> true
  | Tclock_var { tclock_var_def = Some c; _ } -> has_on_clock c
  | Tclock_named (_, c) -> has_on_clock c
  | Tclock_paren c -> has_on_clock c
  | Tclock_tuple clocks -> List.exists (fun clock -> has_on_clock clock) clocks
  | Tclock_function (params, return) ->
      List.exists (fun clock -> has_on_clock clock) params
      || has_on_clock return
  | Tclock_static | Tclock_var { tclock_var_def = None; _ } -> false
