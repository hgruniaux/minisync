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

open Ir

type opt_node_context = {
  mutable onc_refs_memories : MemorySet.t;
      (** Sets of memories referenced in the body of the node. *)
  mutable onc_refs_instances : InstanceSet.t;
      (** Sets of instances referenced in the body of the node. *)
  mutable onc_refs_variables : VariableSet.t;
      (** Sets of variables referenced in the body of the node. *)
}

(** Checks if the expressions [e1] and [e2] are equivalent. *)
let rec are_expressions_equivalent e1 e2 =
  match (e1.iexpr_kind, e2.iexpr_kind) with
  | Iexpr_skip, Iexpr_skip -> true
  | Iexpr_const (Nconst_nil t1), Iexpr_const (Nconst_nil t2) -> Type.equal t1 t2
  | Iexpr_const (Nconst_int n1), Iexpr_const (Nconst_int n2) -> Z.equal n1 n2
  | Iexpr_const (Nconst_float f1), Iexpr_const (Nconst_float f2) ->
      Float.equal f1 f2
  | Iexpr_const (Nconst_constructor c1), Iexpr_const (Nconst_constructor c2) ->
      c1 == c2
  | Iexpr_var v1, Iexpr_var v2 -> v1 == v2
  | Iexpr_state m1, Iexpr_state m2 -> m1 == m2
  | Iexpr_tuple elts1, Iexpr_tuple elts2 ->
      List.length elts1 = List.length elts2
      && List.for_all2 are_expressions_equivalent elts1 elts2
  | Iexpr_binary (op1, l1, r1), Iexpr_binary (op2, l2, r2) ->
      op1 = op2
      && are_expressions_equivalent l1 l2
      && are_expressions_equivalent r1 r2
  | Iexpr_unary (op1, e1), Iexpr_unary (op2, e2) ->
      op1 = op2 && are_expressions_equivalent e1 e2
  | Iexpr_seq exprs1, Iexpr_seq exprs2 ->
      List.length exprs1 = List.length exprs2
      && List.for_all2 are_expressions_equivalent exprs1 exprs2
  | _ -> false

let true_constant = Iexpr_const (Nconst_constructor Type.true_constructor)
let false_constant = Iexpr_const (Nconst_constructor Type.false_constructor)

let is_true_constant expr =
  match expr with
  | Iexpr_const (Nconst_constructor ctor) -> ctor == Type.true_constructor
  | _ -> false

let is_false_constant expr =
  match expr with
  | Iexpr_const (Nconst_constructor ctor) -> ctor == Type.false_constructor
  | _ -> false

let mk_unary_expr op expr =
  Iexpr_unary ({ value = op; loc = Location.dummy }, expr)

let mk_binary_expr op lhs rhs =
  Iexpr_binary ({ value = op; loc = Location.dummy }, lhs, rhs)

let rec optimize_expression (ctx : opt_node_context) expr =
  match expr.iexpr_kind with
  | Iexpr_skip -> expr
  | Iexpr_const _ -> expr
  | Iexpr_var var ->
      ctx.onc_refs_variables <- VariableSet.add var ctx.onc_refs_variables;
      expr
  | Iexpr_func_ref _ -> expr
  | Iexpr_state mem ->
      ctx.onc_refs_memories <- MemorySet.add mem ctx.onc_refs_memories;
      expr
  | Iexpr_tuple elts ->
      mk_iexpr
        (Iexpr_tuple (List.map (optimize_expression ctx) elts))
        expr.iexpr_type
  | Iexpr_seq exprs ->
      let optimized_exprs = optimize_expression_list ctx exprs in
      if List.is_empty optimized_exprs then mk_unit_iexpr Iexpr_skip
      else mk_iexpr (Iexpr_seq optimized_exprs) expr.iexpr_type
  | Iexpr_binary (op, lhs, rhs) ->
      mk_iexpr
        (Iexpr_binary
           (op, optimize_expression ctx lhs, optimize_expression ctx rhs))
        expr.iexpr_type
  | Iexpr_unary (op, expr) ->
      mk_iexpr (Iexpr_unary (op, optimize_expression ctx expr)) expr.iexpr_type
  | Iexpr_call (func, args) ->
      mk_iexpr
        (Iexpr_call
           ( optimize_expression ctx func,
             List.map (optimize_expression ctx) args ))
        expr.iexpr_type
  | Iexpr_match (cond, branches) -> optimize_match_expression ctx cond branches
  | Iexpr_assign (var, value) ->
      mk_iexpr
        (Iexpr_assign (var, optimize_expression ctx value))
        expr.iexpr_type
  | Iexpr_state_assign (mem, value) ->
      mk_iexpr
        (Iexpr_state_assign (mem, optimize_expression ctx value))
        expr.iexpr_type
  | Iexpr_instance_reset inst ->
      ctx.onc_refs_instances <- InstanceSet.add inst ctx.onc_refs_instances;
      expr
  | Iexpr_instance_step (out_vars, inst, args) ->
      ctx.onc_refs_instances <- InstanceSet.add inst ctx.onc_refs_instances;
      mk_iexpr
        (Iexpr_instance_step
           (out_vars, inst, List.map (optimize_expression ctx) args))
        expr.iexpr_type

and optimize_match_expression ctx cond branches =
  let optimized_cond = optimize_expression ctx cond in
  if List.is_empty branches then mk_unit_iexpr Iexpr_skip
  else
    (* Constant folding if the condition is constant. *)
    match optimized_cond.iexpr_kind with
    | Iexpr_const (Nconst_constructor ctor) ->
        (* The typechecker guarantees that the case expression has a branch
               for every constructors available for the condition type. Therefore,
               we are sure to find a matching branch. *)
        let matching_branch =
          List.find (fun (ctor_decl, _) -> ctor_decl == ctor) branches
        in
        let branch_expr = snd matching_branch in
        optimize_expression ctx branch_expr
    | _ ->
        (* Generic case, just try to optimize all branches *)
        let expr_type = (snd (List.hd branches)).iexpr_type in
        mk_iexpr
          (Iexpr_match
             ( optimized_cond,
               List.filter_map
                 (fun (ctor_decl, branch_expr) ->
                   let branch_expr = optimize_expression ctx branch_expr in
                   match branch_expr.iexpr_kind with
                   | Iexpr_skip -> None
                   | _ -> Some (ctor_decl, branch_expr))
                 branches ))
          expr_type

and merge_branches ctx branches1 branches2 =
  match (branches1, branches2) with
  | (ctor, _) :: _, _ | _, (ctor, _) :: _ ->
      let enum_decl = ctor.Tast.tdecl_constructor_enum in
      List.filter_map
        (fun ctor_decl ->
          let branch_expr1 = List.assoc_opt ctor_decl branches1 in
          let branch_expr2 = List.assoc_opt ctor_decl branches2 in
          match (branch_expr1, branch_expr2) with
          | None, None -> None
          | Some be, None -> Some (ctor_decl, be)
          | None, Some be -> Some (ctor_decl, be)
          | Some branch_expr1, Some branch_expr2 ->
              let new_branch_expr =
                mk_iexpr
                  (Iexpr_seq [ branch_expr1; branch_expr2 ])
                  branch_expr2.iexpr_type
              in
              Some (ctor_decl, optimize_expression ctx new_branch_expr))
        enum_decl.Tast.tdecl_enum_constructors
  | [], [] -> []

and merge_match_exprs ctx exprs =
  let rec aux acc = function
    | [] -> List.rev acc
    | { iexpr_kind = Iexpr_match (cond1, branches1); _ }
      :: { iexpr_kind = Iexpr_match (cond2, branches2); iexpr_type }
      :: rest
      when are_expressions_equivalent cond1 cond2 ->
        let merged_branches = merge_branches ctx branches1 branches2 in
        aux acc
          (mk_iexpr (Iexpr_match (cond1, merged_branches)) iexpr_type :: rest)
    | { iexpr_kind = Iexpr_match (cond1, branches1); _ }
      :: {
           iexpr_kind =
             Iexpr_assign
               (var, { iexpr_kind = Iexpr_match (cond2, branches2); _ });
           iexpr_type;
         }
      :: rest
      when are_expressions_equivalent cond1 cond2 ->
        (* A code like
             case v {
               S1 -> e1
               S2 -> e2 }
             x := case v {
               S1 -> e3
               S2 -> e4 }
           can be merged into
             case v {
                S1 -> e1; x := e3
                S2 -> e2; x := e4
             } *)
        let branches2 =
          List.map
            (fun (ctor, expr) ->
              (ctor, mk_iexpr (Iexpr_assign (var, expr)) iexpr_type))
            branches2
        in
        let merged_branches = merge_branches ctx branches1 branches2 in
        aux acc
          (mk_iexpr (Iexpr_match (cond1, merged_branches)) iexpr_type :: rest)
    | {
        iexpr_kind =
          Iexpr_assign (var, { iexpr_kind = Iexpr_match (cond1, branches1); _ });
        iexpr_type = assign_type;
      }
      :: { iexpr_kind = Iexpr_match (cond2, branches2); iexpr_type }
      :: rest
      when are_expressions_equivalent cond1 cond2 ->
        (* Same as the previous case but inversed. *)
        let branches1 =
          List.map
            (fun (ctor, expr) ->
              (ctor, mk_iexpr (Iexpr_assign (var, expr)) assign_type))
            branches1
        in
        let merged_branches = merge_branches ctx branches1 branches2 in
        aux acc
          (mk_iexpr (Iexpr_match (cond1, merged_branches)) iexpr_type :: rest)
    | expr :: rest -> aux (expr :: acc) rest
  in
  aux [] exprs

and optimize_expression_list ctx exprs =
  let optimized_exprs = List.map (optimize_expression ctx) exprs in

  (* Remove "skip" expressions in the sequence. *)
  let optimized_exprs =
    List.filter
      (fun e -> match e.iexpr_kind with Iexpr_skip -> false | _ -> true)
      optimized_exprs
  in

  (* Flatten expressions (unnest nested expressions sequences). *)
  let optimized_exprs =
    List.concat_map
      (fun e ->
        match e.iexpr_kind with
        | Iexpr_seq inner_exprs -> inner_exprs
        | _ -> [ e ])
      optimized_exprs
  in

  (* Merge cases with same condition. *)
  let merged_exprs = merge_match_exprs ctx optimized_exprs in

  merged_exprs

let optimize_node node =
  let ctx =
    {
      onc_refs_memories = MemorySet.empty;
      onc_refs_instances = InstanceSet.empty;
      onc_refs_variables = VariableSet.empty;
    }
  in
  node.inode_reset <- optimize_expression_list ctx node.inode_reset;
  node.inode_step <- optimize_expression_list ctx node.inode_step;
  (* Update the node's memories, instances, and variables based on the
     optimization context. *)
  node.inode_memories <- MemorySet.elements ctx.onc_refs_memories;
  node.inode_instances <- InstanceSet.elements ctx.onc_refs_instances;
  node.inode_variables <- VariableSet.elements ctx.onc_refs_variables
