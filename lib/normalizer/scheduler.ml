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

(* The goal of this module is to schedule the equations in a normalized
   node based on their dependencies. A feasible schedule is one where each
   equation appears after all equations defining variables it depends on
   and that fby equations defining a memory x appears after all equations
   that uses x. The formal definition of a feasible schedule is given in
   the paper "Clock-directed Modular Code Generation for Synchronous Data-flow
   Languages" by D. Biernacki, J.L. Colaço, G. Hamon and M. Pouzet (paragraph
   3).

   The scheduling is done by building a dependency graph of the equations
   and doing a topological sort of this graph. To handle fby equations,
   we add extra dependencies from fby equations defining memories to the
   equations using these memories. This ensures that fby equations appear
   after all equations using the corresponding memory.
 **)

open Nast

module VarSet = Set.Make (struct
  type t = nvar

  let compare v1 v2 = Stdlib.compare v1.nvar_id v2.nvar_id
end)

(** Gets the set of variables used in the given normalized [expr]. *)
let rec uses_in_expr expr =
  match expr.nexpr_kind with
  | Nexpr_const _ -> VarSet.empty
  | Nexpr_var v -> VarSet.singleton v
  | Nexpr_func_ref _ -> VarSet.empty
  | Nexpr_binary (_, lhs, rhs) ->
      VarSet.union (uses_in_expr lhs) (uses_in_expr rhs)
  | Nexpr_unary (_, expr) -> uses_in_expr expr
  | Nexpr_tuple exprs ->
      List.fold_left
        (fun acc e -> VarSet.union acc (uses_in_expr e))
        VarSet.empty exprs
  | Nexpr_call (callee, args) ->
      List.fold_left
        (fun acc e -> VarSet.union acc (uses_in_expr e))
        (uses_in_expr callee) args
  | Nexpr_match (cond, branches) ->
      let acc = uses_in_expr cond in
      List.fold_left
        (fun acc (_, e) -> VarSet.union acc (uses_in_expr e))
        acc branches
  | Nexpr_when (body, _, cond) ->
      VarSet.union (uses_in_expr cond) (uses_in_expr body)

(** Gets the set of variables used in the given normalized [eq]. Only
    instantaneous uses are considered, in particular, uses in the step of a fby
    expression are not considered. *)
let uses_in_eq eq =
  match eq with
  | Neq_assign (_var, expr) -> uses_in_expr expr
  | Neq_fby (_var, _constant, _expr) -> VarSet.empty
  | Neq_every (_vars, _nnode, args, cond, _clock) ->
      List.fold_left
        (fun acc e -> VarSet.union acc (uses_in_expr e))
        (uses_in_expr cond) args

(** Gets the set of variables defined by the given normalized [eq]. *)
let defs_in_eq eq =
  match eq with
  | Neq_assign (var, _expr) -> VarSet.singleton var
  | Neq_fby (var, _constant, _expr) -> VarSet.singleton var
  | Neq_every (vars, _nnode, _args, _cond, _clock) -> VarSet.of_list vars

(* Build a map variable -> index of the equation that defines it.
   We assume normalized equations so each variable is defined at most once. *)
let build_def_map eqs =
  let tbl = Hashtbl.create (List.length eqs * 2) in
  List.iteri
    (fun idx eq ->
      let defs = defs_in_eq eq in
      VarSet.iter (fun v -> Hashtbl.replace tbl v idx) defs)
    eqs;
  tbl

(* Find which variables are defined by fby equations and map them to their eq index *)
let build_fby_def_map eqs =
  let tbl = Hashtbl.create 16 in
  List.iteri
    (fun idx eq ->
      match eq with
      | Neq_fby (var, _c, _e) -> Hashtbl.replace tbl var idx
      | _ -> ())
    eqs;
  tbl

(** Builds the dependency graph for the given list of equations. It returns the
    adjacency list and indegree array. *)
let build_dependency_graph eqs =
  let n = List.length eqs in
  let adj = Array.make n [] in
  let indeg = Array.make n 0 in
  let def_map = build_def_map eqs in
  let fby_map = build_fby_def_map eqs in

  (* Add edges def -> use for syntactic dependencies
     However, if the variable is defined by an fby, skip the def->use edge here.
     For fby-defined variables we will add reader -> fby edges instead to
     ensure readers appear BEFORE the fby (avoid creating def->use and
     reader->fby edges simultaneously, which may produce cycles). *)
  List.iteri
    (fun i eq ->
      let uses = uses_in_eq eq in
      VarSet.iter
        (fun v ->
          (* If v is defined by a fby, skip adding def -> use here. *)
          match Hashtbl.find_opt fby_map v with
          | Some _fby_idx -> ()
          | None -> (
              match Hashtbl.find_opt def_map v with
              | Some def_idx ->
                  (* def_idx -> i *)
                  adj.(def_idx) <- i :: adj.(def_idx);
                  indeg.(i) <- indeg.(i) + 1
              | None -> ()))
        uses)
    eqs;

  (* Add edges reader -> fby for memory update ordering *)
  List.iteri
    (fun i eq ->
      let uses = uses_in_eq eq in
      VarSet.iter
        (fun v ->
          match Hashtbl.find_opt fby_map v with
          | Some fby_idx ->
              (* i -> fby_idx *)
              adj.(i) <- fby_idx :: adj.(i);
              indeg.(fby_idx) <- indeg.(fby_idx) + 1
          | None -> ())
        uses)
    eqs;

  (adj, indeg)

exception Found_cycle

(** Kahn's algorithm for topological sort of equations. Returns Ok order (list
    of indices) or Error with a message on cycle detection. *)
let kahn_topological_sort adj indeg =
  let n = Array.length adj in
  let q = Queue.create () in
  for i = 0 to n - 1 do
    if indeg.(i) = 0 then Queue.add i q
  done;
  let order = ref [] in
  while not (Queue.is_empty q) do
    let v = Queue.pop q in
    order := v :: !order;
    List.iter
      (fun w ->
        indeg.(w) <- indeg.(w) - 1;
        if indeg.(w) = 0 then Queue.add w q)
      adj.(v)
  done;
  if List.length !order <> n then raise Found_cycle else List.rev !order

(** Schedules the equations in the given normalized [node] based on their
    dependencies, such that each equation appears after all equations defining
    variables it depends on and fby equations appear after all equations using
    the corresponding memory variables. The node's equations are updated in
    place. *)
let schedule node =
  let eqs = node.nnode_equations in
  if eqs = [] then ()
  else
    let adj, indeg = build_dependency_graph eqs in
    try
      (* We try to find a topological order of the equations. *)
      let order = kahn_topological_sort adj indeg in
      (* Then we reorder the equations according to the topological order. *)
      let scheduled = List.map (fun idx -> List.nth eqs idx) order in
      node.nnode_equations <- scheduled
    with Found_cycle ->
      failwith
        "Scheduling error: cycle detected in equations dependencies, but \
         should have been detected by typechecker."
