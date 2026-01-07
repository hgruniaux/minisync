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
open Nast

type context = {
  ctx_nodes : (function_declaration, nnode) Hashtbl.t;
  ctx_lets : (let_declaration, nvar) Hashtbl.t;
  ctx_params : (param_declaration, nvar) Hashtbl.t;
}

let fresh_var node typ clock =
  let id = node.nnode_var_cpt in
  node.nnode_var_cpt <- node.nnode_var_cpt + 1;
  { nvar_id = id; nvar_type = typ; nvar_clock = clock }

let add_equation node eq = node.nnode_equations <- eq :: node.nnode_equations

let add_assign_equation node dst expr =
  match expr.nexpr_kind with
  | Nexpr_var src -> src
  | _ ->
      add_equation node (Neq_assign (dst, expr));
      dst

let find_node ctx node_declaration = Hashtbl.find ctx.ctx_nodes node_declaration

let mk_nexpr kind typ clock =
  { nexpr_kind = kind; nexpr_type = typ; nexpr_clock = clock }

let tfalse_expr =
  {
    texpr_kind = Texpr_ref (Tdecl_constructor Type.false_constructor);
    texpr_loc = Location.dummy;
    texpr_type = Type.bool_type;
    texpr_clock = Tclock_static;
  }

let rec normalize_clock ctx node clock =
  match Clock.prune clock with
  | Tast.Tclock_static -> Nclock_static
  | Tast.Tclock_on (parent, ctor, cond) ->
      let nparent = normalize_clock ctx node parent in
      let ncond = normalize_expr ctx node cond in
      Nclock_on (nparent, ctor, ncond)
  | Tast.Tclock_var v -> Nclock_var v.tclock_var_id
  | Tast.Tclock_named (_, clk) -> normalize_clock ctx node clk
  | Tast.Tclock_paren clk -> normalize_clock ctx node clk
  | Tast.Tclock_tuple clks ->
      Nclock_tuple (List.map (normalize_clock ctx node) clks)
  | Tast.Tclock_function (params, ret) ->
      let nparams = List.map (normalize_clock ctx node) params in
      let nret = normalize_clock ctx node ret in
      Nclock_function (nparams, nret)

(** Normalizes an expression within a given node context. *)
and normalize_expr ctx node expr =
  let nclock = normalize_clock ctx node expr.texpr_clock in
  match expr.texpr_kind with
  | Texpr_error -> assert false
  | Texpr_unit ->
      mk_nexpr (Nexpr_const (Nconst_constructor Type.unit_constructor))
        expr.texpr_type nclock
  | Texpr_int n -> mk_nexpr (Nexpr_const (Nconst_int n)) expr.texpr_type nclock
  | Texpr_float n ->
      mk_nexpr (Nexpr_const (Nconst_float n)) expr.texpr_type nclock
  | Texpr_string s ->
      mk_nexpr (Nexpr_const (Nconst_string s)) expr.texpr_type nclock
  | Texpr_char c ->
      mk_nexpr (Nexpr_const (Nconst_char c)) expr.texpr_type nclock
  | Texpr_ref decl -> normalize_ref_expr ctx expr.texpr_type nclock node decl
  | Texpr_func_ref (func_decl, _type_args) ->
      normalize_ref_expr ctx expr.texpr_type nclock node
        (Tdecl_function func_decl)
  | Texpr_tuple exprs ->
      let nexprs = List.map (normalize_expr ctx node) exprs in
      mk_nexpr (Nexpr_tuple nexprs) expr.texpr_type nclock
  | Texpr_paren e -> normalize_expr ctx node e
  | Texpr_seq (first, second) ->
      let _ = normalize_expr ctx node first in
      normalize_expr ctx node second
  | Texpr_let { bindings; body; _ } -> normalize_let_expr ctx node bindings body
  | Texpr_ite (cond, then_branch, else_branch) ->
      normalize_ite_expr ctx expr.texpr_type nclock node cond then_branch
        else_branch
  | Texpr_binary ({ value = Abinop_fby; _ }, initial, step) ->
      normalize_fby_expr ctx expr.texpr_type nclock node initial step
  | Texpr_binary ({ value = Abinop_init; _ }, initial, after) ->
      normalize_init_expr ctx expr.texpr_type nclock node initial after
  | Texpr_binary (op, lhs, rhs) ->
      let nlhs = normalize_expr ctx node lhs in
      let nrhs = normalize_expr ctx node rhs in
      mk_nexpr (Nexpr_binary (op, nlhs, nrhs)) expr.texpr_type nclock
  | Texpr_unary ({ value = Aunop_pre; _ }, expr) ->
      normalize_pre_expr ctx node expr
  | Texpr_unary ({ value = Aunop_last; _ }, expr) ->
      (* TODO: Handle correctly last in case of automatons *)
      normalize_pre_expr ctx node expr
  | Texpr_unary (op, expr) ->
      let nexpr = normalize_expr ctx node expr in
      mk_nexpr (Nexpr_unary (op, nexpr)) expr.texpr_type nclock
  | Texpr_call (callee, _) when Type.is_node callee.texpr_type ->
      normalize_every_expr ctx expr.texpr_type nclock node expr tfalse_expr
  | Texpr_call (callee, args) ->
      normalize_call_expr ctx expr.texpr_type nclock node callee args
  | Texpr_merge (cond, branches, _) ->
      normalize_merge_expr ctx expr.texpr_type nclock node cond branches
  | Texpr_when (body, constructor, cond, _when_loc) ->
      normalize_when_expr ctx expr.texpr_type nclock node body constructor cond
  | Texpr_every (call, cond, _every_loc) ->
      normalize_every_expr ctx expr.texpr_type nclock node call cond

(** Normalizes a constant expression. *)
and normalize_const_expr node expr =
  match expr.texpr_kind with
  | Texpr_int n -> Nconst_int n
  | Texpr_ref (Tdecl_constructor ctor) -> Nconst_constructor ctor
  | Texpr_paren e -> normalize_const_expr node e
  | _ ->
      (* other forms should not appear here as they were rejected by the typechecker *)
      assert false

(** Normalizes a declaration reference expression, except for constructors which
    are constants and are handled somewhere else. *)
and normalize_ref_expr ctx typ clock node decl =
  match decl with
  | Tdecl_let let_decl -> (
      match Hashtbl.find_opt ctx.ctx_lets let_decl with
      | None ->
          let var_clock = normalize_clock ctx node let_decl.tdecl_let_clock in
          let var = fresh_var node let_decl.tdecl_let_type var_clock in
          Hashtbl.add ctx.ctx_lets let_decl var;
          mk_nexpr (Nexpr_var var) typ clock
      | Some var -> mk_nexpr (Nexpr_var var) typ clock)
  | Tdecl_param param_decl ->
      let nvar = Hashtbl.find ctx.ctx_params param_decl in
      mk_nexpr (Nexpr_var nvar) typ clock
  | Tdecl_function func_decl ->
      let nnode = find_node ctx func_decl in
      mk_nexpr (Nexpr_func_ref nnode) typ clock
  | Tdecl_constructor ctor ->
      mk_nexpr (Nexpr_const (Nconst_constructor ctor)) typ clock
  | Tdecl_global _ -> failwith "Global variable not yet supported."

and normalize_let_expr ctx node bindings body =
  List.iter
    (fun let_decl ->
      let nvar =
        match Hashtbl.find_opt ctx.ctx_lets let_decl with
        | None ->
            let var_clock = normalize_clock ctx node let_decl.tdecl_let_clock in
            let var = fresh_var node let_decl.tdecl_let_type var_clock in
            Hashtbl.add ctx.ctx_lets let_decl var;
            var
        | Some v -> v
      in

      let nexpr =
        normalize_expr ctx node (Option.get let_decl.tdecl_let_value)
      in
      let nvar = add_assign_equation node nvar nexpr in
      Hashtbl.replace ctx.ctx_lets let_decl nvar)
    bindings;

  normalize_expr ctx node body

(** Normalizes a combinatorial function application expression. *)
and normalize_call_expr ctx typ clock node callee args =
  assert (not (Type.is_node callee.texpr_type));
  let ncallee = normalize_expr ctx node callee in
  let nargs = List.map (normalize_expr ctx node) args in
  mk_nexpr (Nexpr_call (ncallee, nargs)) typ clock

(** Normalizes a [a fby b] expression. *)
and normalize_fby_expr ctx typ clock node initial step =
  let ninitial = normalize_const_expr node initial in
  let nstep = normalize_expr ctx node step in
  let var_clock = normalize_clock ctx node initial.texpr_clock in
  let var = fresh_var node initial.texpr_type var_clock in
  add_equation node (Neq_fby (var, ninitial, nstep));
  mk_nexpr (Nexpr_var var) typ clock

(** Normalizes a [a -> b] expression. *)
and normalize_init_expr ctx typ clock node initial after =
  (* We rewrite a -> b as
       if true fby false then a else b
     so we only need fby.
     Special case: a -> pre b is equivalent to (a fby b). *)
  match after.texpr_kind with
  | Texpr_unary ({ value = Aunop_pre; _ }, expr)
    when Type.equal expr.texpr_type initial.texpr_type ->
      normalize_fby_expr ctx typ clock node initial expr
  | _ ->
      let cond_var = fresh_var node Type.bool_type Nclock_static in

      add_equation node
        (Neq_fby
           ( cond_var,
             Nconst_constructor Type.true_constructor,
             mk_nexpr (Nexpr_const (Nconst_constructor Type.false_constructor))
               cond_var.nvar_type cond_var.nvar_clock ));

      let ninitial = normalize_expr ctx node initial in
      let nafter = normalize_expr ctx node after in

      mk_nexpr
        (Nexpr_match
           ( mk_nexpr (Nexpr_var cond_var) cond_var.nvar_type cond_var.nvar_clock,
             [
               (Type.true_constructor, ninitial);
               (Type.false_constructor, nafter);
             ] ))
        typ clock

(** Normalizes a [pre a] expression. *)
and normalize_pre_expr ctx node expr =
  (* We rewrite pre x as nil fby x *)
  let nstep = normalize_expr ctx node expr in
  let var_clock = normalize_clock ctx node expr.texpr_clock in
  let var = fresh_var node expr.texpr_type var_clock in

  add_equation node (Neq_fby (var, Nconst_nil expr.texpr_type, nstep));
  mk_nexpr (Nexpr_var var) var.nvar_type var_clock

and normalize_ite_expr ctx typ clock node cond then_branch else_branch =
  let ncond = normalize_expr ctx node cond in

  (* If the condition is constant, simplify the if-then-else expression. *)
  match ncond.nexpr_kind with
  | Nexpr_const (Nconst_constructor ctor) ->
      let false_ctor = Type.falsy_constructor_of cond.texpr_type in
      if ctor == false_ctor then normalize_expr ctx node else_branch
      else normalize_expr ctx node then_branch
  | _ ->
      let nthen = normalize_expr ctx node then_branch in
      let nelse = normalize_expr ctx node else_branch in
      let then_branch = (Type.truthy_constructor_of cond.texpr_type, nthen) in
      let else_branch = (Type.falsy_constructor_of cond.texpr_type, nelse) in
      mk_nexpr (Nexpr_match (ncond, [ then_branch; else_branch ])) typ clock

(** Normalizes a [merge cond (C1 -> ...) ...] expression. *)
and normalize_merge_expr ctx typ clock node cond branches =
  let ncond = normalize_expr ctx node cond in
  let nbranches =
    List.map
      (fun (ctor, expr) ->
        let nexpr = normalize_expr ctx node expr in
        (ctor, nexpr))
      branches
  in
  mk_nexpr (Nexpr_match (ncond, nbranches)) typ clock

(** Normalizes a [body when cond] expression. *)
and normalize_when_expr ctx typ clock node body constructor cond =
  let nbody = normalize_expr ctx node body in
  let ncond = normalize_expr ctx node cond in
  let ncond_var = fresh_var node ncond.nexpr_type ncond.nexpr_clock in
  let ncond_var = add_assign_equation node ncond_var ncond in
  mk_nexpr
    (Nexpr_when
       ( nbody,
         constructor,
         mk_nexpr (Nexpr_var ncond_var) ncond_var.nvar_type ncond_var.nvar_clock
       ))
    typ clock

(** Normalizes a [(f a1 ... an) every clock] expression. *)
and normalize_every_expr ctx typ clock node call cond =
  let node_declaration, args = Tast_utils.extract_function_call call in
  let nargs = List.map (normalize_expr ctx node) args in
  let ncond = normalize_expr ctx node cond in

  let vars, expr =
    match Type.prune typ with
    | Ttype_tuple elts ->
        let elts_clocks =
          match clock with
          | Nclock_tuple elts_clock -> elts_clock
          | _ -> assert false
        in

        let vars =
          List.map2
            (fun elt elt_clock -> fresh_var node elt elt_clock)
            elts elts_clocks
        in
        ( vars,
          Nexpr_tuple
            (List.map
               (fun v -> mk_nexpr (Nexpr_var v) v.nvar_type v.nvar_clock)
               vars) )
    | _ ->
        let var = fresh_var node typ clock in
        ([ var ], Nexpr_var var)
  in

  let nnode = find_node ctx node_declaration in

  add_equation node (Neq_every (vars, nnode, nargs, ncond, clock));
  mk_nexpr expr typ clock

let register_node_or_function ctx node =
  let nnode =
    {
      nnode_tast = node;
      nnode_params = [];
      nnode_equations = [];
      nnode_return = None;
      nnode_var_cpt = 0;
    }
  in

  let params =
    List.mapi
      (fun i param_decl ->
        let param_type = param_decl.tdecl_param_type in
        let param =
          {
            nvar_id = i;
            nvar_type = param_type;
            nvar_clock = normalize_clock ctx nnode param_decl.tdecl_param_clock;
          }
        in
        Hashtbl.add ctx.ctx_params param_decl param;
        param)
      node.tdecl_fun_params
  in

  nnode.nnode_params <- params;
  nnode.nnode_var_cpt <- List.length params;

  Hashtbl.add ctx.ctx_nodes node nnode

let register_declaration ctx decl =
  match decl with
  | Tdecl_function func -> register_node_or_function ctx func
  | _ -> ()

let normalize_node_or_function ctx func =
  let nfunction = Hashtbl.find ctx.ctx_nodes func in
  let nbody = normalize_expr ctx nfunction func.tdecl_fun_body in
  let return_var = fresh_var nfunction nbody.nexpr_type nbody.nexpr_clock in
  let return_var = add_assign_equation nfunction return_var nbody in
  nfunction.nnode_return <- Some return_var;
  if func.tdecl_fun_is_node then Scheduler.schedule nfunction

let normalize_declaration ctx decl =
  match decl with
  | Tdecl_function func -> normalize_node_or_function ctx func
  | _ -> ()

let normalize_program program =
  let ctx =
    {
      ctx_nodes = Hashtbl.create 7;
      ctx_lets = Hashtbl.create 7;
      ctx_params = Hashtbl.create 7;
    }
  in
  (* 1. We first register all declarations so their can refer mutually. *)
  List.iter (register_declaration ctx) program;
  (* 2. Then, we normalize each declarations (globals, functions and nodes). *)
  List.iter (normalize_declaration ctx) program;

  (* 3. Finally, we extract the normalized nodes. *)
  Hashtbl.fold (fun _ nnode acc -> nnode :: acc) ctx.ctx_nodes []
