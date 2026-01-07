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

(* This file is responsible for converting AST types into TAST types,
   ensuring that all type names are valid and defined in the current context.
   It may also contains some utilities functions about types as needed for the other
   typechecker parts. *)

open Ast
open Tast
open Typechecker_common

(** [check_type ctx t] converts an AST type [t] into a TAST type, ensuring that
    all type names are valid and defined in the current context. *)
let rec check_type ctx (t : Ast.atype) =
  match t.value with
  | Atype_int -> Ttype_int
  | Atype_float -> Ttype_float
  | Atype_name id ->
      lookup
        ~error_msg:(fun symbol ->
          (Format.asprintf "Unknown type '%s'." symbol.value, symbol.loc))
        ctx.ctx_types_table id
  | Atype_paren t_inner -> Ttype_paren (check_type ctx t_inner)
  | Atype_tuple types ->
      let ttypes = List.map (check_type ctx) types in
      Ttype_tuple ttypes
  | Atype_function (params, ret) ->
      let param_types = List.map (check_type ctx) params in
      let ret_type = check_type ctx ret in
      Ttype_function (param_types, ret_type)
  | Atype_node (params, ret) ->
      let param_types = List.map (check_type ctx) params in
      let ret_type = check_type ctx ret in
      Ttype_node (param_types, ret_type)
