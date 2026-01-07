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

(** [ignore_parens expr] recursively removes the outer parenthesis of [expr]
    until there is no more. For example, for [Texpr_paren (Texpr_paren (e))], it
    returns [e]. *)
let rec ignore_parens texpr =
  match texpr.texpr_kind with
  | Texpr_paren inner -> ignore_parens inner
  | _ -> texpr

(** [extract_function_declaration expr] extracts the function declaration from
    the expression [expr] (ignore parenthesis). It fails if [expr] is not a
    function reference. *)
let extract_function_declaration texpr =
  match (ignore_parens texpr).texpr_kind with
  | Texpr_func_ref (func_decl, _) -> func_decl
  | Texpr_ref (Tdecl_function func_decl) -> func_decl
  | _ -> failwith "Expected a function declaration reference."

(** [extract_function_call expr] extracts the function declaration and the list
    of arguments from the expression [expr] (ignore parenthesis). It fails if
    [expr] is not a function call. *)
let extract_function_call texpr =
  match (ignore_parens texpr).texpr_kind with
  | Texpr_call (func_expr, args) -> (
      match (ignore_parens func_expr).texpr_kind with
      | Texpr_func_ref (func_decl, _) -> (func_decl, args)
      | Texpr_ref (Tdecl_function func_decl) -> (func_decl, args)
      | _ -> failwith "Expected a function declaration reference.")
  | _ -> failwith "Expected a function call."

(** [location_of_decl decl] returns the location of the declaration [decl]'s
    name. *)
let location_of_decl = function
  | Tdecl_let let_decl -> let_decl.tdecl_let_name.loc
  | Tdecl_param param_decl -> param_decl.tdecl_param_name.loc
  | Tdecl_global global_decl -> global_decl.tdecl_global_name.loc
  | Tdecl_function func_decl -> func_decl.tdecl_fun_name.loc
  | Tdecl_constructor ctor_decl -> ctor_decl.tdecl_constructor_name.loc

(** [type_of_decl decl] returns the type of the declaration [decl]. *)
let type_of_decl = function
  | Tdecl_let let_decl -> let_decl.tdecl_let_type
  | Tdecl_param param_decl -> param_decl.tdecl_param_type
  | Tdecl_global global_decl -> global_decl.tdecl_global_value.texpr_type
  | Tdecl_function func_decl -> func_decl.tdecl_fun_type
  | Tdecl_constructor ctor_decl -> Ttype_enum ctor_decl.tdecl_constructor_enum

let clock_of_decl = function
  | Tdecl_let let_decl -> let_decl.tdecl_let_clock
  | Tdecl_param param_decl -> param_decl.tdecl_param_clock
  | Tdecl_global _ -> Tclock_static
  | Tdecl_function func_decl -> func_decl.tdecl_fun_clock
  | Tdecl_constructor _ctor_decl -> Tclock_static

(** [name_of_decl decl] returns the name of the declaration [decl]. *)
let name_of_decl = function
  | Tdecl_let let_decl -> let_decl.tdecl_let_name.value
  | Tdecl_param param_decl -> param_decl.tdecl_param_name.value
  | Tdecl_global global_decl -> global_decl.tdecl_global_name.value
  | Tdecl_function func_decl -> func_decl.tdecl_fun_name.value
  | Tdecl_constructor ctor_decl -> ctor_decl.tdecl_constructor_name.value

let location_of_type _t = Location.dummy

let has_attribute name attrs =
  List.exists (fun attr -> String.equal attr.Ast.attr_name.value name) attrs

let find_attribute name attrs =
  List.find_opt (fun attr -> String.equal attr.Ast.attr_name.value name) attrs
