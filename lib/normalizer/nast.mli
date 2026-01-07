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

(* This interface defines the normalized abstract syntax tree (NAST) of MiniSync.
 *
 * The NAST is similar to the TAST, but it divides expressions of synchronous
 * nodes into a set of equations defining variables. Moreover, some special
 * synchronous constructs are moved into equations (like fby, every, node calls,
 * etc.). Likewise, some other constructs are simplified and lowered (like match,
 * automatons, pre, init expressions, etc.). Equations are ordered to respect
 * data dependencies between variables, this is done by the scheduler.
 *
 * Non synchronous expressions are mostly unchanged from the TAST, except that
 * variable references now point to normalized variables (nvar) instead of
 * typed variables (tvar). Combinatorial functions are represented as a node
 * with a single equation defining the return variable.
 *
 * The NAST is produced automatically by the normalizer from the TAST.
 * Equations are ordered by the scheduler after normalization.
 *)

open Tast

type nvar = { nvar_id : int; nvar_type : Type.t; nvar_clock : nclock }

and nclock =
  | Nclock_static
  | Nclock_on of nclock * constructor_declaration * nexpression
  | Nclock_var of int
  | Nclock_tuple of nclock list
  | Nclock_function of nclock list * nclock

and nconstant =
  | Nconst_nil of Type.t
  | Nconst_int of Z.t
  | Nconst_float of float
  | Nconst_string of string
  | Nconst_char of char
  | Nconst_constructor of constructor_declaration

and nexpression_kind =
  | Nexpr_var of nvar
  | Nexpr_func_ref of nnode
  | Nexpr_const of nconstant
  | Nexpr_tuple of nexpression list
  | Nexpr_binary of binop * nexpression * nexpression
  | Nexpr_unary of unop * nexpression
  | Nexpr_call of nexpression * nexpression list
  | Nexpr_match of nexpression * (constructor_declaration * nexpression) list
  | Nexpr_when of nexpression * constructor_declaration * nexpression

and nexpression = {
  nexpr_kind : nexpression_kind;
  nexpr_type : Type.t;
  nexpr_clock : nclock;
}

and nequation =
  | Neq_assign of nvar * nexpression
  | Neq_fby of nvar * nconstant * nexpression
  | Neq_every of nvar list * nnode * nexpression list * nexpression * nclock

and nnode = {
  nnode_tast : Tast.function_declaration;
  mutable nnode_params : nvar list;
  mutable nnode_equations : nequation list;
  mutable nnode_return : nvar option;
  mutable nnode_var_cpt : int;
      (** An internal counter to assign fresh ids to variables in this node. *)
}
