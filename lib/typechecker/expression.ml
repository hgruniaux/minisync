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

type t = texpression

(** [is_commutative binop] returns true if the binary operator [binop] is
    commutative. *)
let is_commutative binop =
  let open Ast in
  match binop.value with
  | Abinop_add | Abinop_mul | Abinop_eq | Abinop_ne -> true
  | _ -> false

let rec equal e1 e2 =
  match (e1.texpr_kind, e2.texpr_kind) with
  | Texpr_error, _ -> true
  | _, Texpr_error -> true
  | Texpr_unit, Texpr_unit -> true
  | Texpr_int n1, Texpr_int n2 -> n1 = n2
  | Texpr_float f1, Texpr_float f2 -> f1 = f2
  | Texpr_string s1, Texpr_string s2 -> s1 = s2
  | Texpr_char c1, Texpr_char c2 -> c1 = c2
  | Texpr_ref decl1, Texpr_ref decl2 -> decl1 == decl2
  | Texpr_func_ref (decl1, i1), Texpr_func_ref (decl2, i2) ->
      decl1 == decl2 && i1 == i2
  | Texpr_paren e1, _ -> equal e1 e2
  | _, Texpr_paren e2 -> equal e1 e2
  | Texpr_tuple elems1, Texpr_tuple elems2 -> (
      try List.for_all2 equal elems1 elems2 with Invalid_argument _ -> false)
  | Texpr_seq (e1a, e1b), Texpr_seq (e2a, e2b) -> equal e1a e2a && equal e1b e2b
  | Texpr_ite (cond1, then1, else1), Texpr_ite (cond2, then2, else2) ->
      equal cond1 cond2 && equal then1 then2 && equal else1 else2
  | Texpr_binary (op1, left1, right1), Texpr_binary (op2, left2, right2) ->
      let commutative = is_commutative op1 in
      op1 = op2
      &&
      if commutative then
        (equal left1 left2 && equal right1 right2)
        || (equal left1 right2 && equal right1 left2)
      else equal left1 left2 && equal right1 right2
  | Texpr_unary (op1, expr1), Texpr_unary (op2, expr2) ->
      op1 = op2 && equal expr1 expr2
  | Texpr_call (f1, args1), Texpr_call (f2, args2) -> (
      equal f1 f2
      && try List.for_all2 equal args1 args2 with Invalid_argument _ -> false)
  | Texpr_when (e1, c1, t1, _), Texpr_when (e2, c2, t2, _) ->
      equal e1 e2 && c1 == c2 && equal t1 t2
  | _ -> false

let rec fold ?(ignore_fby = false) f expr acc =
  let acc' = f expr acc in
  match expr.texpr_kind with
  | Texpr_error | Texpr_unit | Texpr_int _ | Texpr_float _ | Texpr_string _
  | Texpr_char _ | Texpr_ref _ | Texpr_func_ref _ ->
      acc'
  | Texpr_paren e -> fold ~ignore_fby f e acc'
  | Texpr_seq (e1, e2) ->
      let acc'' = fold ~ignore_fby f e1 acc' in
      fold ~ignore_fby f e2 acc''
  | Texpr_tuple elems ->
      List.fold_left (fun a e -> fold ~ignore_fby f e a) acc' elems
  | Texpr_call (func, args) ->
      let acc'' = fold ~ignore_fby f func acc' in
      List.fold_left (fun a e -> fold ~ignore_fby f e a) acc'' args
  | Texpr_ite (cond, then_expr, else_expr) ->
      let acc'' = fold ~ignore_fby f cond acc' in
      let acc''' = fold ~ignore_fby f then_expr acc'' in
      fold ~ignore_fby f else_expr acc'''
  | Texpr_binary ({ value = Abinop_fby; _ }, left, _) when ignore_fby ->
      fold ~ignore_fby f left acc'
  | Texpr_binary (_, left, right) ->
      let acc'' = fold ~ignore_fby f left acc' in
      fold ~ignore_fby f right acc''
  | Texpr_unary ({ value = Aunop_pre; _ }, _) when ignore_fby -> acc'
  | Texpr_unary (_, e) -> fold ~ignore_fby f e acc'
  | Texpr_when (e, _, t, _) ->
      let acc'' = fold ~ignore_fby f e acc' in
      fold ~ignore_fby f t acc''
  | Texpr_every (e1, e2, _) ->
      let acc'' = fold ~ignore_fby f e1 acc' in
      fold ~ignore_fby f e2 acc''
  | Texpr_merge (e1, branches, _) ->
      let acc'' = fold ~ignore_fby f e1 acc' in
      List.fold_left
        (fun a (_, br_expr) -> fold ~ignore_fby f br_expr a)
        acc'' branches
  | Texpr_let { bindings; body; _ } ->
      let acc'' =
        List.fold_left
          (fun a binding ->
            match binding.tdecl_let_value with
            | Some bind_expr -> fold ~ignore_fby f bind_expr a
            | None -> a)
          acc' bindings
      in
      fold ~ignore_fby f body acc''
