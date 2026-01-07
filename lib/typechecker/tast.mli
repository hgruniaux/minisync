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

(* This interface defines the typed abstract syntax tree (TAST) of MiniSync.
   The TAST is split in 4 parts:
     - types (native types, aggregate types, variable types, etc.)
     - clocks
     - expressions
     - declarations
   Each texpression has a type and a clock that is inferred/checked by the typechecker.
   A program is seen as a list of declarations.

   The typed AST should not be constructed manually, it is generally provided
   by the typechecker from the raw AST. *)

type binop = Ast.binop
type unop = Ast.unop

(** The types of the MiniSync language. Some types exists only for better
    diagnostics (like [Ttype_named]), or better representation of the original
    spelling (like [Ttype_paren]). *)
type ttype =
  | Ttype_bottom
      (** [Ttype_bottom] represents the bottom type, which has no values.
          Everything is a subtype of [Ttype_bottom]. *)
  | Ttype_var of ttype_var
      (** [Ttype_var v] represents a unification type variable. *)
  | Ttype_int  (** [Ttype_int] is the native integer type. *)
  | Ttype_float  (** [Ttype_float] is the native floating-point type. *)
  | Ttype_string  (** [Ttype_string] is the native string type. *)
  | Ttype_char  (** [Ttype_char] is the native character type. *)
  | Ttype_paren of ttype
      (** [Ttype_paren t] represents a type enclosed in parentheses. *)
  | Ttype_named of Ast.identifier * ttype
      (** [Ttype_named (name, t)] represents a named type with the given
          identifier [name] and the underlying type [t]. This is used to
          preserve the original type name (by a type alias for example). *)
  | Ttype_enum of enum_declaration
      (** [Ttype_enum decl] represents an enumeration type defined by the given
          declaration [decl]. *)
  | Ttype_tuple of ttype list  (** [Ttype_tuple] represents a tuple type. *)
  | Ttype_function of ttype list * ttype
      (** [Ttype_function (params, return)] represents a function type with
          parameter types [params] and return type [return]. *)
  | Ttype_node of ttype list * ttype
      (** [Ttype_node (params, return)] represents a node type with parameter
          types [params] and return type [return]. *)

and ttype_var = {
  ttype_var_id : int;
  mutable ttype_var_def : ttype option;
  mutable ttype_var_level : int;
}

(** The clock types of the MiniSync language. They are similar to types but have
    some differences to represent clock-specific constructs. *)
and tclock =
  | Tclock_var of tclock_var
      (** [Tclock_var v] represents a unification variable clock. *)
  | Tclock_static
      (** [Tclock_static] represents a static clock, which does not depend on
          any other clock or condition. *)
  | Tclock_paren of tclock
      (** [Tclock_paren clock] represents a clock enclosed in parentheses. *)
  | Tclock_named of Ast.identifier * tclock
      (** [Tclock_named (name, clock)] represents a named clock with the given
          identifier [name] and the underlying clock [clock]. *)
  | Tclock_on of tclock * constructor_declaration * texpression
      (** [Tclock_on (clock, constructor, expr)] represents a clock that is
          active when the expression [expr] is equal to [constructor] on the
          given [clock]. *)
  | Tclock_tuple of tclock list
      (** [Tclock_tuple clocks] represents a tuple of clocks. *)
  | Tclock_function of tclock list * tclock
      (** [Tclock_function (params, return)] represents a clock function with
          parameter clocks [params] and return clock [return]. *)

and tclock_var = {
  tclock_var_id : int;
  mutable tclock_var_def : tclock option;
  mutable tclock_var_level : int;
}

(** The expressions of the MiniSync language. Expressions have a type, a clock
    and a optional source location attached which are all recorded in the
    [texpression] type. *)
and texpression_kind =
  | Texpr_error
  | Texpr_unit
  | Texpr_int of Z.t
  | Texpr_float of float
  | Texpr_string of string
  | Texpr_char of char
  | Texpr_ref of declaration
  | Texpr_func_ref of function_declaration * (ttype_var, ttype) Hashtbl.t
  | Texpr_tuple of texpression list
  | Texpr_paren of texpression
  | Texpr_seq of texpression * texpression
  | Texpr_let of {
      bindings : let_declaration list;
      body : texpression;
      rec_loc : Location.t option;
    }
  | Texpr_ite of texpression * texpression * texpression
  | Texpr_binary of binop * texpression * texpression
  | Texpr_unary of unop * texpression
  | Texpr_call of texpression * texpression list
  | Texpr_every of texpression * texpression * Location.t
      (** [Texpr_every (call, cond, every_loc)] represents a 'every' expression
          that execute the node call [call] and resets it every time [cond] is
          true. The [every_loc] indicates the source location of the `every`
          keyword in the original code. *)
  | Texpr_merge of
      texpression * (constructor_declaration * texpression) list * Location.t
      (** [Texpr_merge (cond, branches, merge_loc)] represents a 'merge'
          expression that acts like a case analysis on the value of [cond]. Each
          branch in [branches] corresponds to a constructor and its associated
          expression. The [merge_loc] indicates the source location of the
          'merge' keyword in the original code. *)
  | Texpr_when of
      texpression * constructor_declaration * texpression * Location.t
      (** [Texpr_when (body, constructor, cond, when_loc)] represents a 'when'
          expression where [body] is executed when [cond] matches the given
          [constructor]. The [when_loc] indicates the source location of the
          'when' keyword in the original code. *)

and texpression = {
  texpr_kind : texpression_kind;
  texpr_type : ttype;
  texpr_clock : tclock;
  texpr_loc : Location.t;
}

and let_declaration = {
  tdecl_let_name : Ast.identifier;
  tdecl_let_type : ttype;
  tdecl_let_clock : tclock;
  mutable tdecl_let_value : texpression option;
}

and param_declaration = {
  tdecl_param_name : Ast.identifier;
  tdecl_param_type : ttype;
  tdecl_param_clock : tclock;
}

and global_declaration = {
  tdecl_global_name : Ast.identifier;
  tdecl_global_value : texpression;
  tdecl_global_loc : Location.t;
}

and function_instance = ttype * (ttype_var, ttype) Hashtbl.t * tclock

and deprecated_status =
  | Not_deprecated
  | Deprecated
  | Deprecated_with_message of string

and function_declaration = {
  tdecl_fun_name : Ast.identifier;  (** The function's name. *)
  tdecl_fun_attrs : Ast.attribute list;  (** The function's attributes. *)
  tdecl_fun_type : ttype;
      (** The function's type, including parameter and return types. *)
  tdecl_fun_clock : tclock;
      (** The function's clock, including parameter and return clocks. *)
  tdecl_fun_params : param_declaration list;  (** The function's parameters. *)
  tdecl_fun_body : texpression;  (** The function's body. *)
  tdecl_fun_is_node : bool;
      (** Indicates whether the function is a node (synchronous function) or a
          combinatorial function. *)
  mutable tdecl_fun_deprecated : deprecated_status;
      (** Indicates whether the function is deprecated. *)
  tdecl_fun_loc : Location.t;
      (** The location of the function declaration in the source code. *)
  mutable tdecl_fun_instances : function_instance list;
      (** List of referenced instances of the function's type and clock for
          polymorphic functions/nodes. *)
}

and constructor_declaration = {
  tdecl_constructor_name : Ast.identifier;
  tdecl_constructor_loc : Location.t;
  tdecl_constructor_enum : enum_declaration;
}

and enum_declaration = {
  tdecl_enum_name : Ast.identifier;
  mutable tdecl_enum_constructors : constructor_declaration list;
  tdecl_enum_loc : Location.t;
}

and declaration_kind =
  | Tdecl_let of let_declaration
  | Tdecl_param of param_declaration
  | Tdecl_global of global_declaration
  | Tdecl_function of function_declaration
  | Tdecl_constructor of constructor_declaration

and declaration = declaration_kind

type program = declaration list
