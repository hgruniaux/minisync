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

val ignore_parens : Tast.texpression -> Tast.texpression
(** [ignore_parens expr] recursively removes the outer parenthesis of [expr]
    until there is no more. For example, for [Texpr_paren (Texpr_paren (e))], it
    returns [e]. *)

val extract_function_declaration : Tast.texpression -> Tast.function_declaration
(** [extract_function_declaration expr] extracts the function declaration from
    the expression [expr] (ignore parenthesis). It fails if [expr] is not a
    function reference. *)

val extract_function_call :
  Tast.texpression -> Tast.function_declaration * Tast.texpression list
(** [extract_function_call expr] extracts the function declaration and the list
    of arguments from the expression [expr] (ignore parenthesis). It fails if
    [expr] is not a function call. *)

val location_of_decl : Tast.declaration -> Location.t
(** [location_of_decl decl] returns the location of the declaration [decl]. *)

val type_of_decl : Tast.declaration -> Type.t
(** [type_of_decl decl] returns the type of the declaration [decl]. *)

val clock_of_decl : Tast.declaration -> Clock.t
(** [clock_of_decl decl] returns the clock of the declaration [decl]. *)

val name_of_decl : Tast.declaration -> string
(** [name_of_decl decl] returns the name of the declaration [decl]. *)

val location_of_type : Tast.ttype -> Location.t
(** [location_of_type t] returns the location of the type [t]. *)

val has_attribute : string -> Ast.attribute list -> bool
(** [has_attribute attr attrs] returns true if the attribute [attr] is present
    in the list of attributes [attrs]. *)

val find_attribute : string -> Ast.attribute list -> Ast.attribute option
(** [find_attribute attr attrs] returns [Some attribute] if the attribute [attr]
    is present in the list of attributes [attrs], otherwise returns [None]. *)
