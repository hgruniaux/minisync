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

type t = Tast.tclock

type unification_error =
  | OccursCheck of Tast.tclock_var * t
  | Mismatch of t * t
  | WhenConstructorMismatch of
      Tast.constructor_declaration * Tast.constructor_declaration
  | WhenConditionMismatch of Tast.texpression * Tast.texpression

exception UnificationError of unification_error

val equal : t -> t -> bool
(** [equal c1 c2] checks if the two clocks [c1] and [c2] are equivalent
    (represent the same clock). *)

val prune : ?keep_parens:bool -> t -> t
(** [prune ?keep_parens c] returns the representative of the clock [c],
    following any clock variable bindings. If [keep_parens] is true,
    parenthesesed clocks are preserved. *)

val fresh_var : int -> t
(** [fresh_var level] creates a fresh clock variable with the given [level]. *)

val unify : t -> t -> unit
(** [unify c1 c2] unifies the two clocks [c1] and [c2], making them equivalent.
    Raises [UnificationError] if the clocks cannot be unified. *)

val instantiate : int -> t -> t
(** [instantiate level clock] instantiates all clock variables in [clock] that
    have a level greater than [level], replacing them with fresh clock
    variables. *)

val generalize : int -> t -> Tast.tclock_var list
(** [generalize level clock] generalizes all clock variables in [clock] that
    have a level greater than [level], setting their level to [gvar_level] and
    returning the list of generalized clock variables. *)

val has_on_clock : t -> bool
(** [has_on_clock clock] returns true if the given [clock] contains an 'on'
    clock. *)
