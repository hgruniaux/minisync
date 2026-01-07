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

open Ocaml
open Format

let rec pp_type fmt = function
  | Otype_var n -> fprintf fmt "'a%d" n
  | Otype_unit -> fprintf fmt "unit"
  | Otype_bool -> fprintf fmt "bool"
  | Otype_int -> fprintf fmt "int"
  | Otype_float -> fprintf fmt "float"
  | Otype_char -> fprintf fmt "char"
  | Otype_string -> fprintf fmt "string"
  | Otype_struct s -> fprintf fmt "%s" s.ostruct_name
  | Otype_sum s -> fprintf fmt "%s" s.osum_name
  | Otype_tuple elts ->
      fprintf fmt "(@[<hov>%a@])"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ * ") pp_type)
        elts
  | Otype_function (params, ret) ->
      fprintf fmt "(@[<hov>%a ->@ %a@])"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ->@ ") pp_type)
        params pp_type ret

let rec pp_type_definition fmt = function
  | Otype_var _ | Otype_unit | Otype_bool | Otype_int | Otype_float | Otype_char
  | Otype_string | Otype_tuple _ | Otype_function _ ->
      (* These types cannot be defined in OCaml, they are builtin. *)
      assert false
  | Otype_struct s -> pp_struct_definition fmt s
  | Otype_sum s -> pp_sum_definition fmt s

and pp_struct_definition fmt s =
  fprintf fmt "@[<hov2>%s = {@ %a@ }@]" s.ostruct_name
    (pp_print_list
       ~pp_sep:(fun fmt () -> fprintf fmt ";@ ")
       (fun fmt (name, is_mut, typ) ->
         if is_mut then fprintf fmt "mutable ";
         fprintf fmt "%s : %a" name pp_type typ))
    s.ostruct_fields

and pp_sum_definition fmt s =
  fprintf fmt "@[<hov2>%s =@ %a@]" s.osum_name
    (pp_print_list
       ~pp_sep:(fun fmt () -> fprintf fmt "@ | ")
       (fun fmt name -> fprintf fmt "%s" name))
    s.osum_variants

let pp_type_definitions fmt types =
  fprintf fmt "@[<v>type ";
  pp_print_list
    ~pp_sep:(fun fmt () -> fprintf fmt "@ and ")
    pp_type_definition fmt types;
  fprintf fmt "@]"

let rec pp_pattern fmt = function
  | Opat_unit -> fprintf fmt "()"
  | Opat_var name -> fprintf fmt "%s" name
  | Opat_tuple elts ->
      fprintf fmt "(@[<hov>%a@])"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_pattern)
        elts

let rec pp_expression fmt = function
  | Oexpr_unit -> fprintf fmt "()"
  | Oexpr_bool b -> fprintf fmt "%b" b
  | Oexpr_int n -> Z.pp_print fmt n
  | Oexpr_float n -> fprintf fmt "%f" n
  | Oexpr_char c -> fprintf fmt "%C" c
  | Oexpr_string s -> fprintf fmt "%S" s
  | Oexpr_var v -> fprintf fmt "%s" v
  | Oexpr_tuple elts ->
      fprintf fmt "(@[<hov>%a@])"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_expression)
        elts
  | Oexpr_func f -> fprintf fmt "%s" f.ofunction_name
  | Oexpr_call (callee, args) ->
      fprintf fmt "(@[<hov>%a@ %a@])" pp_expression callee
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ ") pp_expression)
        args
  | Oexpr_struct_create (_, fields) ->
      fprintf fmt "{@[<hv2>%a@]}"
        (pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt ";@ ")
           (fun fmt (name, expr) ->
             fprintf fmt "%s = %a" name pp_expression expr))
        fields
  | Oexpr_struct_field (base, field) ->
      fprintf fmt "(@[<hov>%a@]).%s" pp_expression base field
  | Oexpr_struct_assign (base, value) ->
      fprintf fmt "%a <- %a" pp_expression base pp_expression value
  | Oexpr_let (is_rec, bindings, expr) ->
      fprintf fmt "@[<hov>let ";
      if is_rec then fprintf fmt "rec ";
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt "@ and ")
        (fun fmt (pattern, value) ->
          fprintf fmt "@[<hv2>%a =@ %a@]" pp_pattern pattern pp_expression value)
        fmt bindings;
      fprintf fmt " in@ %a@]" pp_expression expr
  | Oexpr_seq exprs ->
      fprintf fmt "@[<hov>%a@]"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") pp_expression)
        exprs
  | Oexpr_ite (cond, texpr, Oexpr_unit) ->
      fprintf fmt "(@[<hv2>if %a then@ %a@])" pp_expression cond pp_expression
        texpr
  | Oexpr_ite (cond, texpr, fexpr) ->
      fprintf fmt "(@[<hv2>if %a then@ %a else@ %a@])" pp_expression cond
        pp_expression texpr pp_expression fexpr
  | Oexpr_match (cond, branches) ->
      fprintf fmt "(@[<hv2>match %a with@ %a@])" pp_expression cond
        (pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt "@ | ")
           (fun fmt (ctor, expr) ->
             fprintf fmt "@[<hv2>%s ->@ %a@]" ctor pp_expression expr))
        branches
  | Oexpr_unary (op, expr) ->
      fprintf fmt "(@[<hov>%s %a@])" op pp_expression expr
  | Oexpr_binary (op, left, right) ->
      fprintf fmt "(@[<hov>%a %s %a@])" pp_expression left op pp_expression
        right

let pp_function_parameters fmt params =
  match params with
  | [] -> fprintf fmt "()"
  | _ ->
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt " ")
        (fun fmt (name, typ) -> fprintf fmt "(%s : %a)" name pp_type typ)
        fmt params

let pp_function_definition fmt func =
  fprintf fmt "@[<hv-2>%s @[%a@] =@ %a@]" func.ofunction_name
    pp_function_parameters func.ofunction_params pp_expression
    func.ofunction_body

module OFunction = struct
  type t = ofunction

  let equal f1 f2 = f1.ofunction_name = f2.ofunction_name
  let compare f1 f2 = Stdlib.compare f1.ofunction_name f2.ofunction_name
  let hash f = Hashtbl.hash f.ofunction_name
end

module OFunctionGraph = Graph.Imperative.Digraph.Concrete (OFunction)
module OFunctionGraphCycles = Graph.Cycles.Johnson (OFunctionGraph)

let build_call_graph functions =
  let graph = OFunctionGraph.create () in
  List.iter
    (fun func1 ->
      let rec iter_expr = function
        | Oexpr_func func2 -> OFunctionGraph.add_edge graph func1 func2
        | Oexpr_unit | Oexpr_bool _ | Oexpr_int _ | Oexpr_float _ | Oexpr_char _
        | Oexpr_string _ | Oexpr_var _ ->
            ()
        | Oexpr_tuple elts -> List.iter iter_expr elts
        | Oexpr_call (callee, args) ->
            iter_expr callee;
            List.iter iter_expr args
        | Oexpr_struct_create (_, fields) ->
            List.iter (fun (_, expr) -> iter_expr expr) fields
        | Oexpr_struct_field (base, _) -> iter_expr base
        | Oexpr_struct_assign (base, value) ->
            iter_expr base;
            iter_expr value
        | Oexpr_let (_, bindings, expr) ->
            List.iter (fun (_, expr) -> iter_expr expr) bindings;
            iter_expr expr
        | Oexpr_seq exprs -> List.iter iter_expr exprs
        | Oexpr_ite (cond, texpr, fexpr) ->
            iter_expr cond;
            iter_expr texpr;
            iter_expr fexpr
        | Oexpr_match (cond, branches) ->
            iter_expr cond;
            List.iter (fun (_, expr) -> iter_expr expr) branches
        | Oexpr_unary (_, expr) -> iter_expr expr
        | Oexpr_binary (_, left, right) ->
            iter_expr left;
            iter_expr right
      in
      iter_expr func1.ofunction_body)
    functions;
  graph

let pp_function_definitions fmt functions =
  let call_graph = build_call_graph functions in
  let emitted = Hashtbl.create 16 in
  OFunctionGraphCycles.iter_cycles
    (fun cycle ->
      let is_rec =
        match cycle with [] -> assert false | _ :: [] -> false | _ -> true
      in
      fprintf fmt "@[<v>let ";
      if is_rec then fprintf fmt "rec ";
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt "@ and ")
        (fun fmt func ->
          Hashtbl.add emitted func.ofunction_name ();
          pp_function_definition fmt func)
        fmt cycle;
      fprintf fmt "@]@,")
    call_graph;

  List.iter
    (fun func ->
      if not (Hashtbl.mem emitted func.ofunction_name) then (
        fprintf fmt "@[<v>let ";
        pp_function_definition fmt func;
        fprintf fmt "@]@,";
        Hashtbl.add emitted func.ofunction_name ()))
    functions

let pp_module fmt m =
  fprintf fmt "@[<v>";
  fprintf fmt "(* This file was automatically generated by MiniSync *)@,";
  fprintf fmt "(* WARNING: DO NOT MODIFY THIS FILE MANUALLY *)@,";

  fprintf fmt "%a@," pp_type_definitions m.omodule_types_to_be_defined;
  fprintf fmt "%a@," pp_function_definitions m.omodule_functions;

  fprintf fmt "@]"
