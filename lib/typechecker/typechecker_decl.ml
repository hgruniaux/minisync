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

open Ast
open Tast
open Typechecker_common
open Typechecker_expr
open Typechecker_type
open Typechecker_combinatorial
open Typechecker_causality

(** Checks that [name] identifier is not currently used in the global symbol
    table in [ctx]. If it is used, then emits an error about redeclarating an
    identifier. *)
let check_for_redeclaration ctx name =
  match StringMap.find_opt name.value ctx.ctx_decls_table with
  | Some previous_decl ->
      let msg =
        Format.asprintf "Redeclaration of identifier '%s'." name.value
      in
      error_with_note ~ignore_note_if_dummy:true msg name.loc
        "Previously declared here."
        (Tast_utils.location_of_decl previous_decl)
  | None -> ()

(** Adds a declaration to the global symbol table in [ctx]. The function
    [check_for_redeclaration] is called before to checks that we are not
    redefining a symbol. *)
let add_declaration ctx name decl =
  check_for_redeclaration ctx name;
  ctx.ctx_decls_table <- StringMap.add name.value decl ctx.ctx_decls_table

(** Checks the parsed declaration of a global variable. *)
let check_global_declaration ctx name value loc =
  let tvalue = check_expression ctx value in
  ensure_combinatorial_expr tvalue
    ("A global variable must be combinatorial.", Location.dummy);

  let global_declaration =
    {
      tdecl_global_name = name;
      tdecl_global_value = tvalue;
      tdecl_global_loc = loc;
    }
  in

  let decl = Tdecl_global global_declaration in
  add_declaration ctx name decl;
  Some decl

let is_polymorphic func_decl =
  let gvars = Type.generalized_variables func_decl.tdecl_fun_type in
  not (List.is_empty gvars)

(** Checks the [[@test]] attribute. *)
let check_function_test_attribute func_decl attr =
  match attr with
  | { attr_args = _ :: []; attr_name } ->
      error "The 'test' attribute requires no arguments." attr_name.loc
  | { attr_name; _ } when not func_decl.tdecl_fun_is_node ->
      error "The 'test' attribute can only be applied to nodes." attr_name.loc
  | { attr_name; _ } when is_polymorphic func_decl ->
      error "The 'test' attribute cannot be applied to polymorphic nodes."
        attr_name.loc
  | _ -> ()

(** Checks the [[@deprecated]] attribute. *)
let check_deprecated_attribute attr =
  match attr with
  | { attr_args = []; _ } -> Deprecated
  | { attr_args = [ { value = Aexpr_string msg; _ } ]; _ } ->
      Deprecated_with_message msg
  | { attr_args = _; attr_name } ->
      error
        "The 'deprecated' attribute takes either no arguments or a single \
         string argument."
        attr_name.loc

let check_function_attributes func_decl attrs =
  List.iter
    (fun attr ->
      match attr.attr_name.value with
      | "test" -> check_function_test_attribute func_decl attr
      | "deprecated" ->
          func_decl.tdecl_fun_deprecated <- check_deprecated_attribute attr
      | _ ->
          warning
            (Format.asprintf "Unknown function attribute '%s'."
               attr.attr_name.value)
            attr.attr_name.loc)
    attrs

(** Checks the parameters of a function or node declaration. Returns an updated
    typechecking context containing the parameters added to the symbol table, as
    well as the list of typed parameters. *)
let check_parameters ctx params =
  let params_table, tparams =
    List.fold_left_map
      (fun params_table pattern ->
        let name, t =
          match pattern.value with
          | Apat_unit -> (None, Type.unit_type)
          | Apat_wildcard -> (None, Type.fresh_var ctx.ctx_current_level)
          | Apat_ident name -> (Some name, Type.fresh_var ctx.ctx_current_level)
          | Apat_typed ({ value = Apat_ident name; _ }, t) ->
              (Some name, check_type ctx t)
          | _ ->
              error "This pattern not yet supported as parameter." pattern.loc
        in

        let is_unnamed, name =
          match name with
          | Some name -> (false, name)
          | None -> (true, { value = "<anonymous>"; loc = pattern.loc })
        in

        let param =
          {
            tdecl_param_name = name;
            tdecl_param_type = t;
            tdecl_param_clock = Clock.fresh_var ctx.ctx_current_level;
          }
        in

        if not is_unnamed then (
          (if StringMap.mem name.value params_table then
             let msg =
               Format.asprintf "Duplicate parameter name '%s'." name.value
             in
             error msg name.loc);

          (StringMap.add name.value (Tdecl_param param) params_table, param))
        else (params_table, param))
      StringMap.empty params
  in
  ( {
      ctx with
      ctx_decls_table =
        StringMap.union
          (fun _ new_symbol _ -> Some new_symbol)
          params_table ctx.ctx_decls_table;
    },
    tparams )

(** Checks the parsed declaration of a function or node. If [is_node] is true,
    then we are checking a node declaration. *)
let check_function_declaration ~is_node ctx name params body attrs loc =
  let env_level = ctx.ctx_current_level in
  ctx.ctx_current_level <- ctx.ctx_current_level + 1;

  (* Checks parameters and the body. *)
  (if params = [] then
     let msg = if is_node then "Node" else "Function" in
     let msg =
       Format.asprintf "%s '%s' must have at least one parameter." msg
         name.value
     in
     error msg name.loc);

  let body_ctx, tparams = check_parameters ctx params in
  let tbody = check_expression body_ctx body in
  if not is_node then
    ensure_combinatorial_expr tbody
      ( "If you want a sequential function, use 'let node <...>' instead of \
         'let fun <...>'.",
        name.loc )
  else
    (* Check causality of the sequential function. *)
    ignore (schedule tbody);

  (* Build the function type. *)
  let params_types = List.map (fun p -> p.tdecl_param_type) tparams in
  let function_type =
    if is_node then Ttype_node (params_types, tbody.texpr_type)
    else Ttype_function (params_types, tbody.texpr_type)
  in

  let params_clocks = List.map (fun p -> p.tdecl_param_clock) tparams in
  let function_clock = Tclock_function (params_clocks, tbody.texpr_clock) in

  (* We don't want to allow 'on' clocks in inputs (parameters). *)
  List.iter
    (fun param ->
      if Clock.has_on_clock param.tdecl_param_clock then
        let msg =
          Format.asprintf "Parameter '%s' cannot have a sampled clock."
            param.tdecl_param_name.value
        in
        let note =
          Format.asprintf "It has clock '%a'." Tast_printer.pp_clock
            param.tdecl_param_clock
        in
        error_with_note msg param.tdecl_param_name.loc note Location.dummy)
    tparams;

  (* Nor we don't want to allow 'on' clocks in outputs (return type). *)
  (if Clock.has_on_clock tbody.texpr_clock then
     let note =
       Format.asprintf "It has clock '%a'." Tast_printer.pp_clock
         tbody.texpr_clock
     in
     error_with_note
       "Return type cannot have a sampled clock ('when' expression)." body.loc
       note Location.dummy);

  (* Then try to generalize it. *)
  ignore (Clock.generalize env_level function_clock);
  ignore (Type.generalize env_level function_type);

  (* Ensure there is no free type variable in the function type/clock. *)
  let fvars = Type.free_variables function_type in
  (if not (List.is_empty fvars) then
     let msg =
       Format.asprintf
         "Cannot generalize function '%s' because it contains free type \
          variables."
         name.value
     in
     error msg name.loc);

  ctx.ctx_current_level <- ctx.ctx_current_level - 1;

  (* Now that we have the definitive function type, we can declare it! *)
  let function_declaration =
    {
      tdecl_fun_name = name;
      tdecl_fun_attrs = attrs;
      tdecl_fun_type = function_type;
      tdecl_fun_clock = function_clock;
      tdecl_fun_params = tparams;
      tdecl_fun_body = tbody;
      tdecl_fun_deprecated = Not_deprecated;
      tdecl_fun_loc = loc;
      tdecl_fun_is_node = is_node;
      tdecl_fun_instances = [];
    }
  in

  check_function_attributes function_declaration attrs;
  let decl = Tdecl_function function_declaration in
  add_declaration ctx name decl;
  Some decl

(** Checks that [name] is not already used for a type in the current context. If
    it is used, then emits an error about redeclarating a type. *)
let check_for_type_redeclaration ctx name =
  match StringMap.find_opt name.value ctx.ctx_types_table with
  | Some previous_type ->
      let msg = Format.asprintf "Redeclaration of type '%s'." name.value in
      let note_msg = Format.asprintf "Previously declared here." in
      error_with_note ~ignore_note_if_dummy:true msg name.loc note_msg
        (Tast_utils.location_of_type previous_type)
  | None -> ()

let unify_type_to_its_definition ctx name typ =
  let tvar = StringMap.find name.value ctx.ctx_types_table in
  try Type.unify tvar typ
  with Type.UnificationError reason -> (
    let open Type in
    match reason with
    | Mismatch _ | TooFewArguments _ | TooManyArguments _ ->
        (* Impossible here because we are unifying a type variable with
           a type. So the only possible error if its the type variable
           occurs in the second type. *)
        assert false
    | OccursCheck _ ->
        let msg =
          Format.asprintf "Direct recursive type definition for '%s'."
            name.value
        in
        error msg name.loc)

let check_type_alias ctx name aliased_type =
  let t_aliased_type = Ttype_named (name, check_type ctx aliased_type) in
  unify_type_to_its_definition ctx name t_aliased_type;
  None

let check_for_constructor_redeclaration constructors_table name =
  match StringMap.find_opt name.value constructors_table with
  | Some previous_decl ->
      let msg =
        Format.asprintf "Redeclaration of constructor '%s'." name.value
      in
      let note_msg =
        Format.asprintf "Previously declared here in type '%a'."
          Tast_printer.pp_type (Ttype_enum previous_decl.tdecl_constructor_enum)
      in
      error_with_note ~ignore_note_if_dummy:true msg name.loc note_msg
        previous_decl.tdecl_constructor_name.loc
  | None -> ()

let check_type_enum ctx name constructors loc =
  let tenum_declaration =
    {
      tdecl_enum_name = name;
      tdecl_enum_constructors = [];
      tdecl_enum_loc = loc;
    }
  in

  let constructors_table, tconstructors =
    List.fold_left_map
      (fun local_table ctor_id ->
        (match StringMap.find_opt ctor_id.value local_table with
        | Some previous_decl ->
            let msg =
              Format.asprintf "Redeclaration of constructor '%s' in same type."
                ctor_id.value
            in
            let note_msg = "Previously declared here." in
            error_with_note ~ignore_note_if_dummy:true msg ctor_id.loc note_msg
              (Tast_utils.location_of_decl previous_decl)
        | _ -> ());

        let constructor =
          {
            tdecl_constructor_name = ctor_id;
            tdecl_constructor_enum = tenum_declaration;
            tdecl_constructor_loc = ctor_id.loc;
          }
        in

        ( StringMap.add ctor_id.value (Tdecl_constructor constructor) local_table,
          constructor ))
      StringMap.empty constructors
  in
  tenum_declaration.tdecl_enum_constructors <- tconstructors;

  let enum_type = Ttype_enum tenum_declaration in
  unify_type_to_its_definition ctx name enum_type;

  StringMap.iter
    (fun _ decl ->
      match decl with
      | Tdecl_constructor ctor ->
          add_declaration ctx ctor.tdecl_constructor_name decl
      | _ -> assert false)
    constructors_table;

  None

let check_type_definition ctx name type_def =
  match type_def with
  | Atype_decl_alias aliased_type -> check_type_alias ctx name aliased_type
  | Atype_decl_enum constructors ->
      check_type_enum ctx name constructors Location.dummy

let check_type_declaration ctx definitions =
  (* We first create a type variable for each definition so the definitions
     can be mutually recursive dependant. *)
  List.iter
    (fun (name, _) ->
      let tvar = Type.fresh_var ctx.ctx_current_level in
      check_for_type_redeclaration ctx name;
      ctx.ctx_types_table <- StringMap.add name.value tvar ctx.ctx_types_table)
    definitions;

  (* Then we parse and check each type definition (unifying also their type
     variable attached to the name to the defined type). *)
  List.iter
    (fun (name, type_def) -> ignore (check_type_definition ctx name type_def))
    definitions;
  None

let check_declaration ctx (declaration : Ast.declaration) =
  Tast_printer.enter_type_var_context ();
  let declaration, attrs = declaration in
  let tdecl =
    match declaration.value with
    | Adecl_value _ ->
        error "Value declarations are only allowed in signatures."
          declaration.loc
    | Adecl_global (name, value) ->
        check_global_declaration ctx name value declaration.loc
    | Adecl_function (name, params, body, is_node) ->
        check_function_declaration ~is_node ctx name params body attrs
          declaration.loc
    | Adecl_type defs -> check_type_declaration ctx defs
  in
  Tast_printer.exit_type_var_context ();
  tdecl
