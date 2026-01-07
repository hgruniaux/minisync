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

type unification_error =
  | OccursCheck of ttype_var * t
  | Mismatch of t * t
  | TooFewArguments of t * int * int
  | TooManyArguments of t * int * int

exception UnificationError of unification_error

val equal : t -> t -> bool
(** [equal t1 t2] returns true if the two types [t1] and [t2] are equal. *)

val compare : t -> t -> int
(** [compare t1 t2] compares the two types [t1] and [t2]. *)

val prune : ?keep_parens:bool -> t -> t
(** [prune ?keep_parens t] returns the representative type of [t], following any
    type variable bindings. If [keep_parens] is true, parenthesesed types are
    preserved. *)

val fresh_var : int -> t
(** [fresh_var level] creates a fresh type variable with the given [level]. *)

val unify : t -> t -> unit
(** [unify t1 t2] unifies the two types [t1] and [t2], making them equal. Raises
    [UnificationError] if the types cannot be unified. *)

val instantiate : int -> t -> t * (ttype_var, t) Hashtbl.t
(** [instantiate level typ] instantiates all type variables in [typ] that have a
    level greater than [level], replacing them with fresh type variables. It
    also returns the substitution table. *)

val generalize : int -> t -> Tast.ttype_var list
(** [generalize level typ] generalizes all type variables in [typ] that have a
    level greater than [level], setting their level to [gvar_level] and
    returning the list of generalized type variables. *)

val is_generalized : ttype_var -> bool
(** [is_generalized v] returns true if the type variable [v] is generalized. *)

val generalized_variables : t -> Tast.ttype_var list
(** [generalized_variables t] returns the list of generalized variables in the
    type [t]. The return list may have duplicates. *)

val free_variables : t -> Tast.ttype_var list
(** [free_variables t] returns the list of free variables (not generalized and
    not yet unified) in the type [t]. The return list may have duplicates. *)

val is_node : t -> bool
(** [is_node t] returns true if the type [t] is a node type. *)

val is_bool_like : t -> bool
(** [is_bool_like t] returns true if the type [t] is a boolean-like enum type,
    i.e., an enum with exactly two constructors. *)

val truthy_constructor_of : t -> constructor_declaration
(** [truthy_constructor_of t] returns the constructor of the boolean-like type
    [t] corresponding to the true value. *)

val falsy_constructor_of : t -> constructor_declaration
(** [falsy_constructor_of t] returns the constructor of the boolean-like type
    [t] corresponding to the false value. *)

val bool_declaration : enum_declaration
(** [bool_declaration] is the enum declaration for the builtin bool type. *)

val true_constructor : constructor_declaration
(** [true_constructor] is the constructor of the builtin bool type corresponding
    to the true value. *)

val false_constructor : constructor_declaration
(** [false_constructor] is the constructor of the builtin bool type
    corresponding to the false value. *)

val bool_type : t
(** [bool_type] is the type representing the builtin bool type. *)

val unit_declaration : enum_declaration
(** [unit_declaration] is the constructor declaration for the unit type. *)

val unit_constructor : constructor_declaration
(** [unit_constructor] is the constructor of the unit type. *)

val unit_type : t
(** [unit_type] is the type representing the unit type. *)

val is_unit : t -> bool
(** [is_unit t] returns true if the type [t] is the unit type. *)
