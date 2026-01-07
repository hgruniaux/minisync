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

(* This file contains functions to ensure that TAST expressions used in
   combinatorial contexts do not contain disallowed constructs (sequential
   expressions) such as 'pre', 'fby', 'every', 'when', 'merge', or calls
   to nodes. *)

open Tast
open Typechecker_common

(** [not_allowed_in_combinatorial expr_name loc note] raises an error indicating
    that a certain expression is not allowed in combinatorial expressions. *)
let not_allowed_in_combinatorial expr_name loc note =
  let note_msg, note_loc = note in
  error_with_note
    (Format.asprintf
       "'%s' expression is not allowed in combinatorial expressions." expr_name)
    loc note_msg note_loc

(** [ensure_combinatorial_expr texpr note] ensures that the given TAST
    expression [texpr] does not contain any disallowed constructs in
    combinatorial contexts. If it does, an error is raised with the optional
    provided [note]. *)
let rec ensure_combinatorial_expr texpr note =
  match texpr.texpr_kind with
  | Texpr_error | Texpr_unit | Texpr_int _ | Texpr_float _ | Texpr_string _
  | Texpr_char _ ->
      ()
  | Texpr_seq (first, second) ->
      ensure_combinatorial_expr first note;
      ensure_combinatorial_expr second note
  | Texpr_tuple exprs ->
      List.iter (fun e -> ensure_combinatorial_expr e note) exprs
  | Texpr_ref _ | Texpr_func_ref _ -> ()
  | Texpr_paren expr -> ensure_combinatorial_expr expr note
  | Texpr_let { rec_loc = Some rec_loc; _ } ->
      not_allowed_in_combinatorial "recursive let" rec_loc note
  | Texpr_let { bindings; body; rec_loc = None } ->
      List.iter
        (fun decl ->
          let expr = Option.get decl.tdecl_let_value in
          ensure_combinatorial_expr expr note)
        bindings;
      ensure_combinatorial_expr body note
  | Texpr_ite (cond, then_expr, else_expr) ->
      ensure_combinatorial_expr cond note;
      ensure_combinatorial_expr then_expr note;
      ensure_combinatorial_expr else_expr note
  | Texpr_binary ({ value = Abinop_fby; loc }, _, _) ->
      not_allowed_in_combinatorial "fby" loc note
  | Texpr_binary ({ value = Abinop_init; loc }, _, _) ->
      not_allowed_in_combinatorial "->" loc note
  | Texpr_binary (_, left, right) ->
      ensure_combinatorial_expr left note;
      ensure_combinatorial_expr right note
  | Texpr_unary ({ value = Aunop_pre; loc }, _) ->
      not_allowed_in_combinatorial "pre" loc note
  | Texpr_unary ({ value = Aunop_last; loc }, _) ->
      not_allowed_in_combinatorial "last" loc note
  | Texpr_unary (_, expr) -> ensure_combinatorial_expr expr note
  | Texpr_call (callee, args) ->
      if Type.is_node callee.texpr_type then
        error_with_note
          "Calls to nodes are not allowed in combinatorial expressions."
          callee.texpr_loc (fst note) (snd note)
      else (
        ensure_combinatorial_expr callee note;
        List.iter (fun e -> ensure_combinatorial_expr e note) args)
  | Texpr_every (_, _, every_loc) ->
      not_allowed_in_combinatorial "every" every_loc note
  | Texpr_merge (_, _, merge_loc) ->
      not_allowed_in_combinatorial "merge" merge_loc note
  | Texpr_when (_, _, _, when_loc) ->
      not_allowed_in_combinatorial "when" when_loc note
