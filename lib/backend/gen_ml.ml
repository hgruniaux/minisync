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
open Tast
open Ocaml

let instance_name iinst = Format.asprintf "inst%d" iinst.iinst_id
let mem_name imem = Format.asprintf "mem%d" imem.imem_id
let var_name ivar = Format.asprintf "var%d" ivar.ivar_id
let ivar_as_pattern ivar = Opat_var (var_name ivar)
let ivars_as_patterns ivars = Opat_tuple (List.map ivar_as_pattern ivars)

let rec emit_type m = function
  | Ttype_bottom -> assert false
  | Ttype_int -> Otype_int
  | Ttype_float -> Otype_float
  | Ttype_char -> Otype_char
  | Ttype_string -> Otype_string
  | Ttype_tuple elts -> Otype_tuple (List.map (emit_type m) elts)
  | Ttype_function (params, ret) ->
      Otype_function (List.map (emit_type m) params, emit_type m ret)
  | Ttype_paren t -> emit_type m t
  | Ttype_named (_, t) -> emit_type m t
  | Ttype_node _ -> assert false
  | Ttype_var v -> Otype_var v.ttype_var_id
  | Ttype_enum e when e == Type.unit_declaration -> Otype_unit
  | Ttype_enum e when e == Type.bool_declaration -> Otype_bool
  | Ttype_enum e -> (
      match Hashtbl.find_opt m.omodule_sum_types e.tdecl_enum_name.value with
      | Some osum -> Otype_sum osum
      | None ->
          let variants =
            List.map
              (fun c -> c.tdecl_constructor_name.value)
              e.tdecl_enum_constructors
          in

          let osum =
            { osum_name = e.tdecl_enum_name.value; osum_variants = variants }
          in
          Hashtbl.add m.omodule_sum_types e.tdecl_enum_name.value osum;
          Otype_sum osum)

let rec emit_nil t =
  match Type.prune t with
  | Ttype_paren t -> emit_nil t
  | Ttype_named (_, t) -> emit_nil t
  | Ttype_bottom -> failwith "Cannot emit nil for bottom type"
  | Ttype_int -> Oexpr_int Z.zero
  | Ttype_float -> Oexpr_float 0.0
  | Ttype_char -> Oexpr_char '\000'
  | Ttype_string -> Oexpr_string ""
  | Ttype_enum e when e == Type.unit_declaration -> Oexpr_unit
  | Ttype_enum e when e == Type.bool_declaration -> Oexpr_bool false
  | Ttype_enum e -> (
      match e.tdecl_enum_constructors with
      | [] -> failwith "Enum has no constructors"
      | ctor :: _ -> Oexpr_var ctor.tdecl_constructor_name.value)
  | Ttype_tuple elts ->
      let nil_elts = List.map (fun t -> emit_nil t) elts in
      Oexpr_tuple nil_elts
  | Ttype_var _ -> failwith "TODO: emit_nil for type var"
  | _ -> failwith "TODO: emit_nil for other types"

let needs_reset_function node = node.inode_reset <> []
let node_name node = node.inode_nast.nnode_tast.tdecl_fun_name.value
let node_create_method_name node = Format.asprintf "%s_create" (node_name node)
let node_reset_method_name node = Format.asprintf "%s_reset" (node_name node)
let node_step_method_name node = Format.asprintf "%s_step" (node_name node)

let rec node_type m node =
  match Hashtbl.find_opt m.omodule_struct_types (node_name node) with
  | Some ostruct -> Otype_struct ostruct
  | None -> generate_node_struct m node

and generate_node_struct m node =
  let instances_fields =
    List.map
      (fun inst ->
        ( instance_name inst,
          false (* not mutable *),
          node_type m inst.iinst_node ))
      node.inode_instances
  in
  let memories_fields =
    List.map
      (fun mem -> (mem_name mem, true (* mutable *), emit_type m mem.imem_type))
      node.inode_memories
  in
  let all_fields = instances_fields @ memories_fields in
  let ostruct =
    { ostruct_name = node_name node; ostruct_fields = all_fields }
  in
  m.omodule_types_to_be_defined <-
    Otype_struct ostruct :: m.omodule_types_to_be_defined;
  Hashtbl.add m.omodule_struct_types (node_name node) ostruct;
  Otype_struct ostruct

let unop_to_string op =
  let open Ast in
  match op.value with
  | Aunop_neg -> "-"
  | Aunop_not -> "not"
  | Aunop_fneg -> "-."
  | Aunop_last | Aunop_pre -> assert false

let binop_to_string op =
  let open Ast in
  match op.value with
  | Abinop_add -> "+"
  | Abinop_sub -> "-"
  | Abinop_mul -> "*"
  | Abinop_div -> "/"
  | Abinop_mod -> "mod"
  | Abinop_fadd -> "+."
  | Abinop_fsub -> "-."
  | Abinop_fmul -> "*."
  | Abinop_fdiv -> "/."
  | Abinop_eq -> "="
  | Abinop_ne -> "<>"
  | Abinop_lt -> "<"
  | Abinop_le -> "<="
  | Abinop_gt -> ">"
  | Abinop_ge -> ">="
  | Abinop_logand -> "&&"
  | Abinop_logor -> "||"
  | Abinop_fby | Abinop_init -> assert false

let rec emit_expression e =
  match e.iexpr_kind with
  | Iexpr_skip -> Oexpr_unit
  | Iexpr_const (Nconst_int n) -> Oexpr_int n
  | Iexpr_const (Nconst_float n) -> Oexpr_float n
  | Iexpr_const (Nconst_char c) -> Oexpr_char c
  | Iexpr_const (Nconst_string s) -> Oexpr_string s
  | Iexpr_const (Nconst_constructor c) when c == Type.unit_constructor ->
      Oexpr_unit
  | Iexpr_const (Nconst_constructor c) when c == Type.false_constructor ->
      Oexpr_bool false
  | Iexpr_const (Nconst_constructor c) when c == Type.true_constructor ->
      Oexpr_bool true
  | Iexpr_const (Nconst_constructor c) ->
      Oexpr_var c.tdecl_constructor_name.value
  | Iexpr_const (Nconst_nil t) -> emit_nil t
  | Iexpr_seq exprs -> Oexpr_seq (emit_expression_seq exprs)
  | Iexpr_call (callee, args) ->
      Oexpr_call (emit_expression callee, List.map emit_expression args)
  | Iexpr_tuple elts -> Oexpr_tuple (List.map emit_expression elts)
  | Iexpr_unary (op, expr) ->
      Oexpr_unary (unop_to_string op, emit_expression expr)
  | Iexpr_binary (op, left, right) ->
      Oexpr_binary
        (binop_to_string op, emit_expression left, emit_expression right)
  | Iexpr_match (cond, branches) ->
      Oexpr_match
        ( emit_expression cond,
          List.map
            (fun (ctor, expr) ->
              (ctor.tdecl_constructor_name.value, emit_expression expr))
            branches )
  | Iexpr_var var -> Oexpr_var (var_name var)
  | Iexpr_func_ref node -> Oexpr_var (node_name node)
  | Iexpr_assign (var, value) -> emit_assign var value Oexpr_unit
  | Iexpr_state mem -> emit_mem_ref mem
  | Iexpr_state_assign (mem, value) -> emit_state_assign mem value
  | Iexpr_instance_step (vars, i, args) ->
      emit_instance_step vars i args Oexpr_unit
  | Iexpr_instance_reset i ->
      let node = i.iinst_node in
      if needs_reset_function node then
        let inst_ref = emit_instance_ref i in
        Oexpr_call (Oexpr_var (node_reset_method_name node), [ inst_ref ])
      else Oexpr_unit

and emit_mem_ref mem =
  let field_name = mem_name mem in
  Oexpr_struct_field (Oexpr_var "state", field_name)

and emit_instance_ref inst =
  let field_name = instance_name inst in
  Oexpr_struct_field (Oexpr_var "state", field_name)

and emit_state_assign mem value =
  Oexpr_struct_assign (emit_mem_ref mem, emit_expression value)

and emit_assign var value next_inst =
  let value_expr = emit_expression value in
  Oexpr_let (false, [ (ivar_as_pattern var, value_expr) ], next_inst)

and emit_instance_step vars i args next_inst =
  let funcname = node_step_method_name i.iinst_node in
  let inst_ref = emit_instance_ref i in
  let arg_exprs = List.map emit_expression args in
  let pattern = ivars_as_patterns vars in
  let call = Oexpr_call (Oexpr_var funcname, inst_ref :: arg_exprs) in
  Oexpr_let (false, [ (pattern, call) ], next_inst)

and emit_expression_seq exprs =
  match exprs with
  | [] -> []
  | { iexpr_kind = Iexpr_skip; _ } :: rest -> emit_expression_seq rest
  | { iexpr_kind = Iexpr_assign (var, value); _ } :: rest ->
      [ emit_assign var value (emit_expressions rest) ]
  | { iexpr_kind = Iexpr_instance_step (vars, i, args); _ } :: rest ->
      [ emit_instance_step vars i args (emit_expressions rest) ]
  | { iexpr_kind = Iexpr_match (cond, branches); _ } :: rest ->
      (* We do tail-duplication to avoid problems with variables declared in the
         match branches and then used outside of the match.
         Ideally, we should this tail duplication only when necessary (so when [rest]
         expressions references variables defined in the match body). *)
      let obranches =
        List.map
          (fun (ctor, body) ->
            let body_seq =
              match body.iexpr_kind with Iexpr_seq l -> l | _ -> [ body ]
            in
            ( ctor.tdecl_constructor_name.value,
              emit_expressions (body_seq @ rest) ))
          branches
      in
      [ Oexpr_match (emit_expression cond, obranches) ]
  | e :: rest -> (
      match emit_expression e with
      | Oexpr_seq inner_exprs -> inner_exprs @ emit_expression_seq rest
      | Oexpr_unit when rest <> [] -> emit_expression_seq rest
      | expr -> expr :: emit_expression_seq rest)

and emit_expressions exprs =
  let exprs = emit_expression_seq exprs in
  match exprs with [] -> Oexpr_unit | [ expr ] -> expr | _ -> Oexpr_seq exprs

let add_ocaml_function m name params body =
  let func =
    { ofunction_name = name; ofunction_params = params; ofunction_body = body }
  in
  m.omodule_functions <- func :: m.omodule_functions

let generate_function m f =
  assert (not f.inode_nast.nnode_tast.tdecl_fun_is_node);
  let name = f.inode_nast.nnode_tast.tdecl_fun_name.value in
  let params =
    List.map
      (fun ivar -> (var_name ivar, emit_type m ivar.ivar_type))
      f.inode_params
  in
  let body = emit_expressions f.inode_step in
  add_ocaml_function m name params body

let generate_node_create_function m node =
  assert node.inode_nast.nnode_tast.tdecl_fun_is_node;
  let name = node_create_method_name node in
  let params = [] in
  let body = Oexpr_unit in
  add_ocaml_function m name params body

let generate_node_reset_function m node =
  assert node.inode_nast.nnode_tast.tdecl_fun_is_node;
  let name = node_reset_method_name node in
  let params = [ ("state", node_type m node) ] in
  let body = emit_expressions node.inode_reset in
  add_ocaml_function m name params body

let generate_node_step_function m node =
  assert node.inode_nast.nnode_tast.tdecl_fun_is_node;
  let name = node_step_method_name node in
  let params =
    ("state", node_type m node)
    :: List.map
         (fun ivar -> (var_name ivar, emit_type m ivar.ivar_type))
         node.inode_params
  in
  let body = emit_expressions node.inode_step in
  add_ocaml_function m name params body

let generate_node m node =
  generate_node_create_function m node;
  if needs_reset_function node then generate_node_reset_function m node;
  generate_node_step_function m node

let emit_program tast =
  let m =
    {
      omodule_types_to_be_defined = [];
      omodule_sum_types = Hashtbl.create 16;
      omodule_struct_types = Hashtbl.create 16;
      omodule_functions = [];
    }
  in
  List.iter
    (fun n ->
      let is_node = n.inode_nast.nnode_tast.tdecl_fun_is_node in
      if is_node then generate_node m n else generate_function m n)
    tast;
  m
