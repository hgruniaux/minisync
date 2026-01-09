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

(* This interface defines the abstract syntax tree (AST) of MiniSync.
   The definition tries to closely follow the concrete syntax of the language.

   The AST will be directly produced by the parser, and consumed by the typechecker.
   Each expression, types and declaration is annotated with its source location for
   precise error reporting and tooling support. *)

type 'a with_location = { value : 'a; loc : Location.t }
type identifier = string with_location

type atype_kind =
  | Atype_autocomplete of string option * string option
      (** [Atype_autocomplete (prefix, suffix)] represents an autocomplete
          placeholder for types. If [prefix] is [Some s], it indicates that the
          user has already typed the prefix [s]. Same for [suffix]. *)
  | Atype_int
  | Atype_float
  | Atype_name of identifier
  | Atype_paren of atype
  | Atype_tuple of atype list
  | Atype_function of atype list * atype
  | Atype_node of atype list * atype

and atype = atype_kind with_location

type aclock_kind =
  | Aclock_name of identifier
  | Aclock_paren of aclock
  | Aclock_static
  | Aclock_tuple of aclock list
  | Aclock_function of aclock list * aclock

and aclock = aclock_kind with_location

type binop_kind =
  | Abinop_add
  | Abinop_sub
  | Abinop_mul
  | Abinop_div
  | Abinop_mod
  | Abinop_fadd
  | Abinop_fsub
  | Abinop_fmul
  | Abinop_fdiv
  | Abinop_logand
  | Abinop_logor
  | Abinop_eq
  | Abinop_ne
  | Abinop_lt
  | Abinop_le
  | Abinop_gt
  | Abinop_ge
  | Abinop_init
  | Abinop_fby

and binop = binop_kind with_location

type unop_kind = Aunop_neg | Aunop_fneg | Aunop_not | Aunop_pre | Aunop_last
and unop = unop_kind with_location

type expression_kind =
  | Aexpr_autocomplete of string option * string option
      (** [Aexpr_autocomplete (prefix, suffix)] represents an autocomplete
          placeholder for expressions. If [prefix] is [Some s], it indicates
          that the user has already typed the prefix [s]. Same for [suffix]. *)
  | Aexpr_unit  (** [Aexpr_unit] represents the unit literal '()'. *)
  | Aexpr_int of Z.t
      (** [Aexpr_int n] represents an integer literal with value [n]. *)
  | Aexpr_float of float
      (** [Aexpr_float f] represents a floating-point literal with value [f]. *)
  | Aexpr_string of string
      (** [Aexpr_string s] represents a string literal with value [s]. *)
  | Aexpr_char of char
      (** [Aexpr_char c] represents a character literal with value [c]. *)
  | Aexpr_var of identifier
      (** [Aexpr_var id] represents a variable with identifier [id]. *)
  | Aexpr_tuple of expression list
      (** [Aexpr_tuple exprs] represents a tuple expression containing the
          expressions in [exprs]. *)
  | Aexpr_paren of expression
      (** [Aexpr_paren expr] represents a parenthesized expression [expr]. *)
  | Aexpr_let of let_binding list * expression * Location.t option
      (** [Aexpr_let (bindings, body, rec_loc)] represents a 'let' expression
          that defines local bindings in [bindings] used in [body]. If [rec_loc]
          is [Some loc], the bindings are recursive, with [loc] indicating the
          source location of the 'rec' keyword. *)
  | Aexpr_where of let_binding list * expression * Location.t option
      (** [Aexpr_where (bindings, body, rec_loc)] represents a 'where'
          expression that defines local bindings in [bindings] used in [body].
          If [rec_loc] is [Some loc], the bindings are recursive, with [loc]
          indicating the source location of the 'rec' keyword. *)
  | Aexpr_ite of expression * expression * expression
      (** [Aexpr_ite (cond, then_branch, else_branch)] represents an
          if-then-else expression. *)
  | Aexpr_binary of binop * expression * expression
      (** [Aexpr_binary (op, lhs, rhs)] represents a binary operation . *)
  | Aexpr_unary of unop * expression
      (** [Aexpr_unary (op, expr)] represents a unary operation. *)
  | Aexpr_call of expression * expression list
      (** [Aexpr_call (callee, args)] represents a function call . *)
  | Aexpr_when of expression * identifier * expression * Location.t
      (** [Aexpr_when (expr, constructor, cond, when_loc)] represents a 'when'
          expression where [expr] is executed when [cond] matches the given
          [constructor]. The [when_loc] indicates the source location of the
          'when' keyword in the original code. *)
  | Aexpr_when_bool of expression * expression * Location.t
      (** [Aexpr_when_bool (body, cond, when_loc)] represents a 'when'
          expression where [body] is executed when the boolean [cond] is true.
          The [when_loc] indicates the source location of the 'when' keyword in
          the original code. *)
  | Aexpr_every of expression * expression * Location.t
      (** [Aexpr_every (call_expr, clock_expr, every_loc)] represents an 'every'
          expression that executes [call_expr] and resets the underlying node on
          the ticks of [clock_expr]. The [every_loc] indicates the source
          location of the 'every' keyword in the original code. *)
  | Aexpr_merge of expression * (pattern * expression) list * Location.t
      (** [Aexpr_merge (cond, branches, merge_loc)] represents a 'merge'
          expression that selects one of the [branches] based on the value of
          [cond]. Each branch is associated with a constructor pattern. The
          [merge_loc] indicates the source location of the 'merge' keyword in
          the original code. *)
  | Aexpr_seq of expression * expression
      (** [Aexpr_seq (first, second)] represents a sequential composition of
          expressions where [first] is evaluated before [second]. *)
  | Aexpr_match of expression * (pattern * expression) list
      (** [Aexpr_match (expr, branches)] represents a match expression that
          matches [expr] against multiple [branches], each consisting of a
          pattern and the corresponding expression to execute. *)

and preemption_item = bool * bool * expression * identifier
(** [(is_unless, is_continue, cond, constructor)] represents a state transition
    condition. *)

and let_binding = pattern * expression

and pattern_kind =
  | Apat_unit
  | Apat_wildcard
  | Apat_ident of identifier
  | Apat_typed of pattern * atype
  | Apat_tuple of pattern list

and pattern = pattern_kind with_location
and expression = expression_kind with_location

type parameter = identifier * atype option
type attribute = { attr_name : identifier; attr_args : expression list }

type type_declaration_kind =
  | Atype_decl_alias of atype
  | Atype_decl_enum of identifier list

type declaration_kind =
  | Adecl_value of identifier * atype
  | Adecl_global of identifier * expression
  | Adecl_function of identifier * pattern list * expression * bool
      (** [Adecl_function (name, params, body, is_node)] *)
  | Adecl_type of (identifier * type_declaration_kind) list

and declaration = declaration_kind with_location * attribute list

type program = declaration list
