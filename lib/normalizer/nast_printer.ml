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

open Nast
open Format
open Tast
open Printer_utils

let pp_variable =
  pp_with_color Variable (fun fmt v -> fprintf fmt "v%d" v.nvar_id)

let pp_constructor =
  pp_with_color Constructor (fun fmt ctor ->
      fprintf fmt "%s" ctor.tdecl_constructor_name.value)

let pp_constant fmt = function
  | Nconst_nil _ -> pp_string_with_color Constant fmt "nil"
  | Nconst_int n -> pp_int fmt n
  | Nconst_float n -> pp_float fmt n
  | Nconst_string s -> pp_string fmt s
  | Nconst_char c -> pp_char fmt c
  | Nconst_constructor ctor -> pp_constructor fmt ctor

let rec pp_clock fmt clock =
  let saved_print_clocks = !Printer_utils.print_clocks in
  Printer_utils.print_clocks := false;
  (match clock with
  | Nclock_static -> pp_keyword fmt "static"
  | Nclock_on (clk, ctor, cond) ->
      fprintf fmt "%a %a %a(%a)" pp_clock clk pp_keyword "on" pp_constructor
        ctor pp_expression cond
  | Nclock_var v -> fprintf fmt "'a%d" v
  | Nclock_tuple clocks ->
      fprintf fmt "(@[<hov>%a@])"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_clock)
        clocks
  | Nclock_function (params, ret) ->
      fprintf fmt "(@[<hov>%a ->@ %a@])"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ->@ ") pp_clock)
        params pp_clock ret);
  Printer_utils.print_clocks := saved_print_clocks

and pp_expression fmt expr =
  let aux fmt expr =
    match expr.nexpr_kind with
    | Nexpr_var v -> pp_variable fmt v
    | Nexpr_func_ref nnode ->
        let name = nnode.nnode_tast.tdecl_fun_name.value in
        pp_string_with_color Function fmt name
    | Nexpr_const c -> pp_constant fmt c
    | Nexpr_tuple exprs ->
        fprintf fmt "(@[<hov>%a@])"
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
             pp_expression)
          exprs
    | Nexpr_binary (op, lhs, rhs) ->
        fprintf fmt "(@[<hov>%a %a@ %a@])" pp_expression lhs
          Tast_printer.pp_binop op pp_expression rhs
    | Nexpr_unary (op, expr) ->
        fprintf fmt "(@[<hov>%a %a@])" Tast_printer.pp_unop op pp_expression
          expr
    | Nexpr_call (callee, args) ->
        fprintf fmt "@[%a@](@[<hov2>%a@])" pp_expression callee
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
             pp_expression)
          args
    | Nexpr_match (cond, [ (c1, b1); (c2, b2) ])
      when c1 == Type.true_constructor && c2 == Type.false_constructor ->
        (* Special pretty print for (if ... then ... else ...) *)
        fprintf fmt "(@[<hv>@[<v2>%a %a %a@ %a@]@ @[<v2>%a@ %a@]@])" pp_keyword
          "if" pp_expression cond pp_keyword "then" pp_expression b1 pp_keyword
          "else" pp_expression b2
    | Nexpr_match (cond, branches) ->
        fprintf fmt "(@[<hov2>%a @[%a@] %a@ %a@])" pp_keyword "match"
          pp_expression cond pp_keyword "with"
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt "@ | ")
             (fun fmt (ctor, expr) ->
               fprintf fmt "@[<hov2>%a ->@ %a@]" pp_constructor ctor
                 pp_expression expr))
          branches
    | Nexpr_when (body, constructor, cond) ->
        fprintf fmt "(@[<hv2>%a %a@ %a %a(%a)@])" pp_keyword "when"
          pp_expression body pp_keyword "on" pp_constructor constructor
          pp_expression cond
  in
  if !Printer_utils.print_clocks then
    let comment = Format.asprintf "(* %a *)" pp_clock expr.nexpr_clock in
    fprintf fmt "%a %a" aux expr pp_comment comment
  else aux fmt expr

let pp_equation fmt eq =
  let pp_def_var fmt var =
    if !Printer_utils.print_types then
      fprintf fmt "(%a : %a)" pp_variable var Tast_printer.pp_type var.nvar_type
    else pp_variable fmt var
  in

  let pp_def_vars fmt vars =
    if !Printer_utils.print_types then
      fprintf fmt "(%a : %a)"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_variable)
        vars Tast_printer.pp_type
        (Ttype_tuple (List.map (fun v -> v.nvar_type) vars))
    else
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_variable)
        fmt vars
  in

  match eq with
  | Neq_assign (var, expr) ->
      fprintf fmt "%a = %a" pp_def_var var pp_expression expr
  | Neq_fby (var, constant, expr) ->
      fprintf fmt "%a = %a %a %a" pp_def_var var pp_constant constant pp_keyword
        "fby" pp_expression expr
  | Neq_every (vars, nnode, args, cond, _clock) ->
      let name = nnode.nnode_tast.tdecl_fun_name.value in
      fprintf fmt "%a = (@[%a %a@]) %a %a" pp_def_vars vars
        (pp_string_with_color Function)
        name
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ ") pp_expression)
        args pp_keyword "every" pp_expression cond

let pp_node fmt node =
  Tast_printer.enter_type_var_context ();
  let name = node.nnode_tast.tdecl_fun_name.value in
  let is_node = node.nnode_tast.tdecl_fun_is_node in
  fprintf fmt "@[<v>%a %a(@[<hov>%a@])" pp_keyword
    (if is_node then "node" else "function")
    (pp_string_with_color Function)
    name
    (pp_print_list
       ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
       (fun fmt v ->
         fprintf fmt "%a%a" pp_variable v
           (fun fmt t ->
             if !Printer_utils.print_types then
               fprintf fmt ": %a" Tast_printer.pp_type t
             else ())
           v.nvar_type))
    node.nnode_params;

  if !Printer_utils.print_types then
    let return = Option.get node.nnode_return in
    fprintf fmt " : %a = " Tast_printer.pp_type return.nvar_type
  else fprintf fmt " = ";

  (if is_node then (
     pp_variable fmt (Option.get node.nnode_return);
     if node.nnode_equations <> [] then
       fprintf fmt "@;@[<v2>%a@,%a@]" pp_keyword "where"
         (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,") pp_equation)
         node.nnode_equations)
   else
     match node.nnode_equations with
     | [ Neq_assign (ret_var, body) ]
       when ret_var == Option.get node.nnode_return ->
         fprintf fmt "@;@[<v2>  %a@]" pp_expression body
     | _ ->
         (* Combinatorial function ill-formed! *)
         assert false);

  fprintf fmt "@]";
  Tast_printer.exit_type_var_context ()

let pp_program fmt nodes =
  fprintf fmt "@[<v>";
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,@,") pp_node fmt nodes;
  fprintf fmt "@]"
