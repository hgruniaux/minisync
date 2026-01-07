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
open Typechecker_common

module LetVarSet = Set.Make (struct
  type t = let_declaration

  let compare = Stdlib.compare
end)

(** Returns all referenced instantaneous variables (not in pre, last, etc.).
    [uses_locs] is a hashtable that will map each variable to its usages source
    locations in [expr]. *)
let referenced_instant_vars uses_locs expr =
  Expression.fold ~ignore_fby:true
    (fun e acc ->
      match e.texpr_kind with
      | Texpr_ref (Tdecl_let var) ->
          Hashtbl.add uses_locs var e.texpr_loc;
          LetVarSet.add var acc
      | _ -> acc)
    expr LetVarSet.empty

(** Collects all let bindings in a expression, recursively. *)
let collect_let_bindings expr =
  Expression.fold
    (fun e acc ->
      match e.texpr_kind with
      | Texpr_let { bindings; _ } -> bindings @ acc
      | _ -> acc)
    expr []

(** Builds the dependency graph for let bindings in the expression. For each let
    binding, it maps the binding to the set of let bindings it depends on
    instantaneously (not in pre, last, etc.). *)
let build_dependency_graph expr =
  let bindings = collect_let_bindings expr in
  let graph = Hashtbl.create 7 in
  List.iter
    (fun binding ->
      let expr = Option.get binding.tdecl_let_value in
      let uses_locs = Hashtbl.create 7 in
      let referenced_vars = referenced_instant_vars uses_locs expr in
      Hashtbl.add graph binding (referenced_vars, uses_locs))
    bindings;

  graph

exception Found_cycle of let_declaration list

(** Tries to perform a topological sort on the dependency graph of let bindings.
    If a cycle is found, [Found_cycle] is raised with the cycle. *)
let topological_sort graph =
  let visited = Hashtbl.create 7 in
  let temp_mark = Hashtbl.create 7 in
  let sorted = ref [] in

  (* We compute the topological order by depth-first search.
     If we encounter a node that is temporarily marked, it means there is a cycle. *)
  let rec visit node =
    if Hashtbl.mem temp_mark node then raise (Found_cycle [ node ])
    else if not (Hashtbl.mem visited node) then (
      Hashtbl.add temp_mark node ();
      let dependencies, _ =
        Hashtbl.find_opt graph node
        |> Option.value ~default:(LetVarSet.empty, Hashtbl.create 0)
      in
      LetVarSet.iter
        (fun dep ->
          try visit dep
          with Found_cycle cycle -> raise (Found_cycle (node :: cycle)))
        dependencies;
      Hashtbl.remove temp_mark node;
      Hashtbl.add visited node ();
      sorted := node :: !sorted)
  in

  Hashtbl.iter (fun node _ -> visit node) graph;
  List.rev !sorted

(** Tries to schedule the equations in the expression. The output result, even
    if correct, is generally useless as scheduling is redone after
    normalization. However, this function emits an error in case of a cyclic
    dependency and because it operates on the typed AST, it is able to provide
    more precise error messages. *)
let schedule expr =
  let graph = build_dependency_graph expr in
  try topological_sort graph
  with Found_cycle cycle ->
    (* Bad, bad, bad... We have found a cyclic dependency.
       We try as best as we can to provide a helpful error message. *)
    let head = List.hd cycle in
    let msg =
      "The program is not causal, this variable has a cyclic dependency."
    in
    (* Creates a sequence of notes to explain the dependency cycle for the user. *)
    let notes =
      let rec aux previous_d acc = function
        | [] -> acc
        | d :: tl ->
            let _, uses_locs = Hashtbl.find graph previous_d in
            let previous_name = previous_d.tdecl_let_name.value in
            let current_name = d.tdecl_let_name.value in
            let note_msg =
              Format.asprintf "The variable '%s' depends on '%s' here."
                previous_name current_name
            in
            let note_loc = Hashtbl.find uses_locs d in
            aux d ((note_msg, note_loc) :: acc) tl
      in
      aux (List.hd cycle) [] (List.tl cycle)
    in

    raise (Error (msg, head.tdecl_let_name.loc, List.rev notes))
