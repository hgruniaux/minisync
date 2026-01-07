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
open Nast

type ivar_or_imem = IVar of ivariable | IMem of imemory

type context = {
  ctx_nodes : (nnode, inode) Hashtbl.t;
  ctx_vars : (nvar, ivar_or_imem) Hashtbl.t;
  ctx_conds : (Tast.texpression, iexpression) Hashtbl.t;
}

let optimize_ir = ref false

(** Generates a new fresh local variable of given [typ] in [node]. *)
let fresh_variable node typ =
  let id = node.inode_var_cpt in
  node.inode_var_cpt <- node.inode_var_cpt + 1;
  let var = { ivar_id = id; ivar_type = typ } in
  node.inode_variables <- var :: node.inode_variables;
  var

(** Generates a new fresh memory/state variable of given [typ] in [node]. *)
let fresh_memory node typ =
  let id = node.inode_mem_cpt in
  node.inode_mem_cpt <- node.inode_mem_cpt + 1;
  let mem = { imem_id = id; imem_type = typ } in
  node.inode_memories <- mem :: node.inode_memories;
  mem

(** Adds the statement [stmt] at the end of [node]'s reset function. *)
let add_reset_statement node stmt = node.inode_reset <- stmt :: node.inode_reset

(** Adds the statement [stmt] at the end of [node]'s step function. *)
let add_step_statement node stmt = node.inode_step <- stmt :: node.inode_step

let codegen_nvar_as_memory ctx node var =
  match Hashtbl.find_opt ctx.ctx_vars var with
  | Some (IMem m) -> m
  | Some (IVar _) -> failwith "Variable is a variable, not a memory"
  | None ->
      let mem = fresh_memory node var.nvar_type in
      Hashtbl.add ctx.ctx_vars var (IMem mem);
      mem

let codegen_nvar_as_variable ctx node var =
  match Hashtbl.find_opt ctx.ctx_vars var with
  | Some (IVar v) -> v
  | Some (IMem _) -> failwith "Variable is a memory, not a variable"
  | None ->
      let v = fresh_variable node var.nvar_type in
      Hashtbl.add ctx.ctx_vars var (IVar v);
      v

let lookup_var ctx node var =
  match Hashtbl.find_opt ctx.ctx_vars var with
  | Some (IVar v) -> mk_iexpr (Iexpr_var v) v.ivar_type
  | Some (IMem m) -> mk_iexpr (Iexpr_state m) m.imem_type
  | None ->
      mk_iexpr (Iexpr_var (codegen_nvar_as_variable ctx node var)) var.nvar_type

let lookup_node ctx nnode = Hashtbl.find ctx.ctx_nodes nnode

let mk_constant constant =
  let typ =
    match constant with
    | Nconst_constructor ctor -> Tast.Ttype_enum ctor.tdecl_constructor_enum
    | Nconst_nil t -> t
    | Nconst_int _ -> Tast.Ttype_int
    | Nconst_float _ -> Tast.Ttype_float
    | Nconst_string _ -> Tast.Ttype_string
    | Nconst_char _ -> Tast.Ttype_char
  in
  mk_iexpr (Iexpr_const constant) typ

let codegen_nnode_as_instance ctx parent_node nnode =
  let inode = Hashtbl.find ctx.ctx_nodes nnode in
  let id = parent_node.inode_inst_cpt in
  parent_node.inode_inst_cpt <- parent_node.inode_inst_cpt + 1;
  let inst = { iinst_id = id; iinst_node = inode } in
  parent_node.inode_instances <- inst :: parent_node.inode_instances;
  inst

(** Generates an if-then-else expression. *)
let codegen_ite cond cond_type ~true_body ~false_body =
  let enum_decl =
    match Type.prune cond_type with
    | Tast.Ttype_enum enum_decl -> enum_decl
    | _ -> failwith "Unsupported condition type in guard"
  in

  (* By convention, the false contructor is the first one, and the
     true constructor is the second one. *)
  match enum_decl.tdecl_enum_constructors with
  | [ false_constructor; true_constructor ] -> (
      (* We optimize the guard if the condition is constant!
         if true then A else B == A
         if false then A else B == B *)
      match cond.iexpr_kind with
      | Iexpr_const (Nconst_constructor ctor) ->
          if ctor == true_constructor then true_body else false_body
      | _ ->
          mk_iexpr
            (Iexpr_match
               ( cond,
                 [
                   (true_constructor, true_body); (false_constructor, false_body);
                 ] ))
            true_body.iexpr_type)
  | _ ->
      (* We expect a boolean-like enum with only two constructors.
         This is ensured by the type checker. *)
      assert false

(** Guards [expr] with the given clock. *)
let rec guard_clock ctx node expr clock =
  match clock with
  | Nclock_on (_, ctor, cond) ->
      let icond = codegen_nexpression ctx node cond in
      add_step_statement node
        (mk_iexpr (Iexpr_match (icond, [ (ctor, expr) ])) expr.iexpr_type)
  | _ -> add_step_statement node expr

and codegen_nexpression ctx node expr =
  match expr.nexpr_kind with
  | Nexpr_const c -> mk_iexpr (Iexpr_const c) expr.nexpr_type
  | Nexpr_var v -> lookup_var ctx node v
  | Nexpr_func_ref nnode ->
      mk_iexpr (Iexpr_func_ref (lookup_node ctx nnode)) expr.nexpr_type
  | Nexpr_binary (op, lhs, rhs) ->
      assert (op.value <> Ast.Abinop_init && op.value <> Ast.Abinop_fby);
      let ilhs = codegen_nexpression ctx node lhs in
      let irhs = codegen_nexpression ctx node rhs in
      mk_iexpr (Iexpr_binary (op, ilhs, irhs)) expr.nexpr_type
  | Nexpr_unary (op, expr) ->
      assert (op.value <> Ast.Aunop_pre);
      let iexpr = codegen_nexpression ctx node expr in
      mk_iexpr (Iexpr_unary (op, iexpr)) expr.nexpr_type
  | Nexpr_tuple exprs ->
      let iexprs = List.map (codegen_nexpression ctx node) exprs in
      mk_iexpr (Iexpr_tuple iexprs) expr.nexpr_type
  | Nexpr_call (callee, args) ->
      let icallee = codegen_nexpression ctx node callee in
      let iargs = List.map (codegen_nexpression ctx node) args in
      mk_iexpr (Iexpr_call (icallee, iargs)) expr.nexpr_type
  | Nexpr_match (cond, branches) ->
      let icond = codegen_nexpression ctx node cond in
      let ibranches =
        List.map
          (fun (ctor, expr) -> (ctor, codegen_nexpression ctx node expr))
          branches
      in
      mk_iexpr (Iexpr_match (icond, ibranches)) expr.nexpr_type
  | Nexpr_when (body, _constructor, _ncond) ->
      (* (match Clock.prune expr.nexpr_clock with
      | Tast.Tclock_on (_, _ctor, tcond) ->
          Hashtbl.add ctx.ctx_conds tcond (lookup_var ctx node ncond)
      | _ -> assert false); *)
      codegen_nexpression ctx node body

let codegen_equation ctx node eq =
  match eq with
  | Neq_assign (var, expr) ->
      let var = codegen_nvar_as_variable ctx node var in
      guard_clock ctx node
        (mk_iexpr
           (Iexpr_assign (var, codegen_nexpression ctx node expr))
           var.ivar_type)
        expr.nexpr_clock
  | Neq_fby (var, constant, expr) ->
      let mem = codegen_nvar_as_memory ctx node var in
      let value = codegen_nexpression ctx node expr in
      add_reset_statement node
        (mk_iexpr
           (Iexpr_state_assign (mem, mk_constant constant))
           mem.imem_type);
      guard_clock ctx node
        (mk_iexpr (Iexpr_state_assign (mem, value)) mem.imem_type)
        expr.nexpr_clock
  | Neq_every (vars, nnode, args, cond, clock) ->
      let ivars = List.map (codegen_nvar_as_variable ctx node) vars in
      let iargs = List.map (codegen_nexpression ctx node) args in
      let icond = codegen_nexpression ctx node cond in
      let iinstance = codegen_nnode_as_instance ctx node nnode in
      add_reset_statement node (mk_unit_iexpr (Iexpr_instance_reset iinstance));
      guard_clock ctx node
        (codegen_ite icond cond.nexpr_type
           ~true_body:(mk_unit_iexpr (Iexpr_instance_reset iinstance))
           ~false_body:(mk_unit_iexpr Iexpr_skip))
        clock;
      guard_clock ctx node
        (mk_iexpr
           (Iexpr_instance_step (ivars, iinstance, iargs))
           Type.unit_type)
        clock

let register_function_or_node ctx nnode =
  let params =
    List.mapi
      (fun i nvar ->
        let ivar = { ivar_id = i; ivar_type = nvar.nvar_type } in
        Hashtbl.add ctx.ctx_vars nvar (IVar ivar);
        ivar)
      nnode.nnode_params
  in

  let inode =
    {
      inode_nast = nnode;
      inode_params = params;
      inode_return_type = (Option.get nnode.nnode_return).nvar_type;
      inode_variables = [];
      inode_memories = [];
      inode_instances = [];
      inode_reset = [];
      inode_step = [];
      inode_var_cpt = List.length params;
      inode_mem_cpt = 0;
      inode_inst_cpt = 0;
    }
  in
  Hashtbl.add ctx.ctx_nodes nnode inode

let codegen_node ctx nnode =
  let inode = Hashtbl.find ctx.ctx_nodes nnode in

  (* Predeclaration of variables and memories. *)
  List.iter
    (fun eq ->
      match eq with
      | Neq_fby (var, _, _) -> ignore (codegen_nvar_as_memory ctx inode var)
      | Neq_assign (var, _) -> ignore (codegen_nvar_as_variable ctx inode var)
      | Neq_every (vars, _, _, _, _) ->
          List.iter
            (fun var -> ignore (codegen_nvar_as_variable ctx inode var))
            vars)
    nnode.nnode_equations;

  List.iter (codegen_equation ctx inode) nnode.nnode_equations;

  (* Reverse the statements to preserve the original order *)
  let return_expr = lookup_var ctx inode (Option.get nnode.nnode_return) in
  inode.inode_reset <- List.rev inode.inode_reset;
  inode.inode_step <- List.rev (return_expr :: inode.inode_step);

  if !optimize_ir then Ir_optimizer.optimize_node inode;
  inode

let codegen_function_or_node ctx nnode =
  if nnode.nnode_tast.tdecl_fun_is_node then codegen_node ctx nnode
  else codegen_node ctx nnode

let codegen_program nprogram =
  let ctx =
    {
      ctx_nodes = Hashtbl.create 17;
      ctx_vars = Hashtbl.create 17;
      ctx_conds = Hashtbl.create 17;
    }
  in
  List.iter (register_function_or_node ctx) nprogram;
  List.map (codegen_function_or_node ctx) nprogram
