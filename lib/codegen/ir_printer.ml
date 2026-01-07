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
open Format
open Printer_utils

let pp_variable =
  pp_with_color Variable (fun fmt v -> fprintf fmt "v%d" v.ivar_id)

let pp_memory =
  pp_with_color Variable (fun fmt mem -> fprintf fmt "m%d" mem.imem_id)

let pp_instance =
  pp_with_color Variable (fun fmt inst -> fprintf fmt "inst%d" inst.iinst_id)

let pp_constructor = Nast_printer.pp_constructor
let pp_constant = Nast_printer.pp_constant

let rec pp_expression fmt expr =
  let aux fmt expr =
    match expr.iexpr_kind with
    | Iexpr_skip -> pp_keyword fmt "skip"
    | Iexpr_const c -> pp_constant fmt c
    | Iexpr_var v -> pp_variable fmt v
    | Iexpr_func_ref nnode ->
        let name = nnode.inode_nast.nnode_tast.tdecl_fun_name.value in
        pp_string_with_color Function fmt name
    | Iexpr_state v -> fprintf fmt "%a(%a)" pp_keyword "state" pp_memory v
    | Iexpr_tuple exprs ->
        fprintf fmt "(@[<hov>%a@])"
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
             pp_expression)
          exprs
    | Iexpr_seq exprs ->
        fprintf fmt "(@[<hov>%a@])"
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt ";@ ")
             pp_expression)
          exprs
    | Iexpr_binary (op, lhs, rhs) ->
        fprintf fmt "(@[<hov>%a %a@ %a@])" pp_expression lhs
          Tast_printer.pp_binop op pp_expression rhs
    | Iexpr_unary (op, expr) ->
        fprintf fmt "(@[<hov>%a %a@])" Tast_printer.pp_unop op pp_expression
          expr
    | Iexpr_call (callee, args) ->
        fprintf fmt "@[%a@](@[<hov2>%a@])" pp_expression callee
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
             pp_expression)
          args
    | Iexpr_match (cond, branches) ->
        fprintf fmt "(@[<hv2>%a @[%a@] %a@ %a@])" pp_keyword "match"
          pp_expression cond pp_keyword "with"
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt "@ | ")
             (fun fmt (ctor, expr) ->
               fprintf fmt "@[<hv2>%a ->@ %a@]" pp_constructor ctor
                 pp_expression expr))
          branches
    | Iexpr_assign (var, expr) ->
        fprintf fmt "%a := %a" pp_variable var pp_expression expr
    | Iexpr_state_assign (mem, expr) ->
        fprintf fmt "%a(%a) := %a" pp_keyword "state" pp_memory mem
          pp_expression expr
    | Iexpr_instance_reset instance ->
        fprintf fmt "%a.%a" pp_instance instance pp_keyword "reset"
    | Iexpr_instance_step (vars, instance, args) ->
        fprintf fmt "%a := %a.%a(@[<hov>%a@])"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_variable)
          vars pp_instance instance pp_keyword "step"
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
             pp_expression)
          args
  in
  aux fmt expr

let pp_variable_decl fmt var =
  if !Printer_utils.print_types then
    fprintf fmt "%a : %a" pp_variable var Tast_printer.pp_type var.ivar_type
  else pp_variable fmt var

let pp_memory_decl fmt mem =
  if !Printer_utils.print_types then
    fprintf fmt "%a : %a" pp_memory mem Tast_printer.pp_type mem.imem_type
  else pp_memory fmt mem

let pp_instance_decl fmt inst =
  if !Printer_utils.print_types then
    let name = inst.iinst_node.inode_nast.nnode_tast.tdecl_fun_name.value in
    fprintf fmt "%a : %a %a(@[<hov>%a@]) -> %a" pp_instance inst pp_keyword
      "node"
      (pp_string_with_color Function)
      name
      (pp_print_list
         ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
         (fun fmt param ->
           if !Printer_utils.print_types then
             fprintf fmt "%a : %a" pp_variable param Tast_printer.pp_type
               param.ivar_type
           else pp_variable fmt param))
      inst.iinst_node.inode_params Tast_printer.pp_type
      inst.iinst_node.inode_return_type
  else pp_instance fmt inst

let pp_node fmt node =
  Tast_printer.enter_type_var_context ();
  let name = node.inode_nast.nnode_tast.tdecl_fun_name.value in
  fprintf fmt "@[<v>%a %a(@[<hov>%a@])" pp_keyword "node"
    (pp_string_with_color Function)
    name
    (pp_print_list
       ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
       (fun fmt param ->
         if !Printer_utils.print_types then
           fprintf fmt "%a : %a" pp_variable param Tast_printer.pp_type
             param.ivar_type
         else pp_variable fmt param))
    node.inode_params;

  if !Printer_utils.print_types then
    fprintf fmt " : %a =@;" Tast_printer.pp_type node.inode_return_type
  else fprintf fmt " =@;";

  (* Print variables. *)
  if node.inode_variables <> [] then
    fprintf fmt "@[<v2>%a@,%a@]@," pp_keyword "variables"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,") pp_variable_decl)
      node.inode_variables;

  (* Print memories. *)
  if node.inode_memories <> [] then
    fprintf fmt "@[<v2>%a@,%a@]@," pp_keyword "memories"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,") pp_memory_decl)
      node.inode_memories;

  (* Print instances. *)
  if node.inode_instances <> [] then
    fprintf fmt "@[<v2>%a@,%a@]@," pp_keyword "instances"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,") pp_instance_decl)
      node.inode_instances;

  (* Print the reset method body. *)
  if node.inode_reset <> [] then
    fprintf fmt "@[<v2>%a@,%a@]@," pp_keyword "reset"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@,") pp_expression)
      node.inode_reset
  else
    fprintf fmt "%a %a@," pp_keyword "reset" Printer_utils.pp_comment
      "(* empty *)";

  (* Print the step method body. *)
  if node.inode_step <> [] then
    fprintf fmt "@[<v2>%a@,%a@]" pp_keyword "step"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@,") pp_expression)
      node.inode_step
  else
    fprintf fmt "%a %a" pp_keyword "step" Printer_utils.pp_comment "(* empty *)";
  fprintf fmt "@]";
  Tast_printer.exit_type_var_context ()

let pp_function fmt func =
  Tast_printer.enter_type_var_context ();
  (* Function signature (name + parameters) *)
  let name = func.inode_nast.nnode_tast.tdecl_fun_name.value in
  fprintf fmt "@[<v>%a %a(@[<hov>%a@])" pp_keyword "function"
    (pp_string_with_color Function)
    name
    (pp_print_list
       ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
       (fun fmt param ->
         if !Printer_utils.print_types then
           fprintf fmt "%a : %a" pp_variable param Tast_printer.pp_type
             param.ivar_type
         else pp_variable fmt param))
    func.inode_params;

  (* Function return type *)
  if !Printer_utils.print_types then
    fprintf fmt " : %a =@;" Tast_printer.pp_type func.inode_return_type
  else fprintf fmt " =@;";

  (* Print the step method body. *)
  if func.inode_step <> [] then
    fprintf fmt "@[<v2>  %a@]"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@,") pp_expression)
      func.inode_step
  else Printer_utils.pp_comment fmt "(* empty *)";
  fprintf fmt "@]"

let pp_node_or_function fmt node =
  let is_node = node.inode_nast.nnode_tast.tdecl_fun_is_node in
  if is_node then pp_node fmt node else pp_function fmt node

let pp_program fmt program =
  fprintf fmt "@[<v>";
  pp_print_list
    ~pp_sep:(fun fmt () -> fprintf fmt "@,@,")
    pp_node_or_function fmt program;
  fprintf fmt "@]"
