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
open Tast_utils

(** Lookup a constructor declaration by its identifier in the given context. If
    it doesn't find the identifier in the context or if it doesn't refer to a
    constructor, then this function emit an error. *)
let lookup_constructor ctx id =
  let decl =
    lookup
      ~error_msg:(fun symbol ->
        (Format.asprintf "Unbound identifier '%s'." symbol.value, symbol.loc))
      ~bonus:(fun decl -> match decl with Tdecl_constructor _ -> 1 | _ -> -1)
      ctx.ctx_decls_table id
  in
  match decl with
  | Tdecl_constructor ctor_decl -> ctor_decl
  | _ ->
      let msg =
        Format.asprintf "Identifier '%s' is not a constructor." id.value
      in
      let note_msg =
        Format.asprintf "Identifier '%s' refers to a %s." id.value
          (match decl with
          | Tdecl_let _ -> "let binding"
          | Tdecl_param _ -> "parameter"
          | Tdecl_global _ -> "global"
          | Tdecl_function d when d.tdecl_fun_is_node -> "node"
          | Tdecl_function _ -> "function"
          | Tdecl_constructor _ -> assert false)
      in
      error_with_note msg id.loc note_msg id.loc

(** Ensures [texpr] has boolean-like type, and if not, emit an error.

    A type is considered boolean-like if it is an enum with exactly two
    constructors. In that case, the first constructor is treated as false and
    the second as true. *)
let expect_boolean_expr _ctx texpr =
  (* We ignore parens for better locations in error messages. *)
  let texpr = ignore_parens texpr in
  match Type.prune texpr.texpr_type with
  | Ttype_var _ ->
      (* Type variable not yet unified, force unification with bool! *)
      Type.unify texpr.texpr_type Type.bool_type
  | Ttype_enum enum_decl -> (
      match enum_decl.tdecl_enum_constructors with
      | [ _ctor1; _ctor2 ] -> ()
      | _ ->
          (* We have a enum type but it does not have exactly two constructors. *)
          let msg =
            Format.asprintf "Expected boolean-like expression, but got '%a'."
              Tast_printer.pp_type texpr.texpr_type
          in
          let note_msg =
            Format.asprintf
              "Type '%a' is not boolean-like, it has %d constructors instead \
               of 2."
              Tast_printer.pp_type texpr.texpr_type
              (List.length enum_decl.tdecl_enum_constructors)
          in
          error_with_note msg texpr.texpr_loc note_msg
            enum_decl.tdecl_enum_name.loc)
  | _ ->
      (* Not even a enum type... *)
      let msg =
        Format.asprintf "Expected boolean-like expression, but got '%a'."
          Tast_printer.pp_type texpr.texpr_type
      in
      error msg texpr.texpr_loc

(** Ensures [texpr] has an enum type and returns it, and if not, emit an error
    message. *)
let expect_enum_expr texpr =
  match Type.prune texpr.texpr_type with
  | Ttype_enum enum_declaration -> enum_declaration
  | _ ->
      let msg =
        Format.asprintf "Expected enum type, but got '%a'." Tast_printer.pp_type
          texpr.texpr_type
      in
      error msg texpr.texpr_loc

(** Try to unify the types of two expressions. If unification fails, emit an
    error. *)
let unify_types_in_expressions texpr1 texpr2 =
  let texpr1 = ignore_parens texpr1 in
  let texpr2 = ignore_parens texpr2 in
  try Type.unify texpr1.texpr_type texpr2.texpr_type
  with Type.UnificationError reason -> (
    (* Unification failed, try to provide the best error message possible. *)
    match reason with
    | Type.OccursCheck _ ->
        let msg =
          Format.asprintf
            "Cyclic dependency detected between type '%a' and type '%a'."
            Tast_printer.pp_type texpr2.texpr_type Tast_printer.pp_type
            texpr1.texpr_type
        in
        let note_msg =
          Format.asprintf
            "Unifying these types would create an infinite type, which is not \
             supported."
        in
        error_with_note ~ignore_note_if_dummy:true msg texpr1.texpr_loc note_msg
          texpr2.texpr_loc
    | Type.Mismatch (t1, t2) ->
        let msg =
          Format.asprintf "Cannot unify type '%a' with type '%a'."
            Tast_printer.pp_type texpr2.texpr_type Tast_printer.pp_type
            texpr1.texpr_type
        in
        let note_msg =
          Format.asprintf "Type '%a' does not match type '%a'."
            Tast_printer.pp_type t1 Tast_printer.pp_type t2
        in
        error_with_note ~ignore_note_if_dummy:true msg texpr1.texpr_loc note_msg
          texpr2.texpr_loc
    | Type.TooFewArguments (t, expected_args, given_args) ->
        let msg =
          Format.asprintf
            "Cannot unify type '%a': expected %d arguments but got %d."
            Tast_printer.pp_type t expected_args given_args
        in
        error msg texpr1.texpr_loc
    | Type.TooManyArguments (t, expected_args, given_args) ->
        let msg =
          Format.asprintf
            "Cannot unify type '%a': expected %d arguments but got %d."
            Tast_printer.pp_type t expected_args given_args
        in
        error msg texpr1.texpr_loc)

(** Try to unify the clocks of two expressions. If unification fails, emit an
    error. *)
let unify_clocks_in_expressions texpr1 texpr2 =
  let texpr1 = ignore_parens texpr1 in
  let texpr2 = ignore_parens texpr2 in
  try Clock.unify texpr1.texpr_clock texpr2.texpr_clock
  with Clock.UnificationError reason -> (
    (* Unification failed, try to provide the best error message possible. *)
    match reason with
    | Clock.OccursCheck _ ->
        let msg =
          Format.asprintf
            "Cyclic dependency detected between clock '%a' and clock '%a'."
            Tast_printer.pp_clock texpr2.texpr_clock Tast_printer.pp_clock
            texpr1.texpr_clock
        in
        let note_msg =
          Format.asprintf
            "Unifying these clocks would create an infinite clock, which is \
             not supported."
        in
        error_with_note ~ignore_note_if_dummy:true msg texpr1.texpr_loc note_msg
          texpr2.texpr_loc
    | Clock.Mismatch (c1, c2) ->
        let msg =
          Format.asprintf "Cannot unify clock '%a' with clock '%a'."
            Tast_printer.pp_clock texpr2.texpr_clock Tast_printer.pp_clock
            texpr1.texpr_clock
        in
        let note_msg =
          Format.asprintf "Clock '%a' does not match clock '%a'."
            Tast_printer.pp_clock c1 Tast_printer.pp_clock c2
        in
        error_with_note ~ignore_note_if_dummy:true msg texpr1.texpr_loc note_msg
          texpr2.texpr_loc
    | Clock.WhenConditionMismatch (cond1, cond2) ->
        let msg =
          Format.asprintf "Cannot unify clock '%a' with clock '%a'."
            Tast_printer.pp_clock texpr1.texpr_clock Tast_printer.pp_clock
            texpr2.texpr_clock
        in
        let first_note = Format.asprintf "This 'when' condition, ..." in
        let second_note =
          Format.asprintf "..., is not equivalent to this one."
        in
        error_with_two_notes ~ignore_note_if_dummy:true msg texpr2.texpr_loc
          first_note cond1.texpr_loc second_note cond2.texpr_loc
    | Clock.WhenConstructorMismatch (ctor1, ctor2) ->
        let msg =
          Format.asprintf "Cannot unify clock '%a' with clock '%a'."
            Tast_printer.pp_clock texpr1.texpr_clock Tast_printer.pp_clock
            texpr2.texpr_clock
        in
        let note_msg =
          Format.asprintf
            "Clock on constructor '%s' does not match clock on constructor \
             '%s'."
            ctor1.tdecl_constructor_name.value
            ctor2.tdecl_constructor_name.value
        in
        error_with_note ~ignore_note_if_dummy:true msg texpr1.texpr_loc note_msg
          texpr2.texpr_loc)

(** Returns the most general common clock between [clock1] and [clock2]. *)
let common_clock clock1 clock2 =
  match (Clock.prune clock1, Clock.prune clock2) with
  | Tclock_static, c | c, Tclock_static -> c
  | c, _ -> c

(** Try to unify the types and clocks of two expressions. If unification fails,
    emit an error. It calls [unify_types_in_expressions] and
    [unify_clocks_in_expressions]. *)
let unify_expressions texpr1 texpr2 =
  (* We ignore parens for better locations in error messages. *)
  let texpr1 = ignore_parens texpr1 in
  let texpr2 = ignore_parens texpr2 in
  unify_types_in_expressions texpr1 texpr2;
  unify_clocks_in_expressions texpr1 texpr2

let expect_type texpr expected_type =
  try Type.unify texpr.texpr_type expected_type
  with Type.UnificationError _ ->
    let msg =
      Format.asprintf "Expected type '%a', but got type '%a'."
        Tast_printer.pp_type expected_type Tast_printer.pp_type texpr.texpr_type
    in
    error msg texpr.texpr_loc

(** Expects [texpr] to be a constant expression. If not, emit an error. *)
let rec expect_constant texpr =
  match (ignore_parens texpr).texpr_kind with
  | Texpr_int _ -> ()
  | Texpr_float _ -> ()
  | Texpr_ref (Tdecl_constructor _) -> ()
  | Texpr_paren e -> expect_constant e
  | Texpr_tuple elements -> List.iter expect_constant elements
  | _ ->
      let msg = Format.asprintf "Expected constant expression." in
      error msg texpr.texpr_loc

let rec check_expression ctx (main_expr : Ast.expression) =
  try
    let loc = main_expr.loc in
    match main_expr.value with
    | Aexpr_autocomplete (prefix, suffix) ->
        assert !in_autocomplete_mode;
        Autocompleter.autocomplete_expr ctx loc prefix suffix
    | Aexpr_unit -> check_unit_expression loc
    | Aexpr_int value -> check_integer_expression loc value
    | Aexpr_float value -> check_float_expression loc value
    | Aexpr_string value -> check_string_expression loc value
    | Aexpr_char value -> check_char_expression loc value
    | Aexpr_tuple elements -> check_tuple_expression ctx loc elements
    | Aexpr_paren e -> check_paren_expression ctx loc e
    | Aexpr_var id -> check_variable_expression ctx loc id
    | Aexpr_let (bindings, body, rec_loc) ->
        check_let_expression ctx loc bindings body rec_loc
    | Aexpr_where (bindings, body, rec_loc) ->
        (* FIXME: have special Texpr_where instead of reusing Texpr_let *)
        check_let_expression ctx loc bindings body rec_loc
    | Aexpr_binary (op, lhs, rhs) -> check_binary_expression ctx loc op lhs rhs
    | Aexpr_unary (op, expr) -> check_unary_expression ctx loc op expr
    | Aexpr_ite (cond, then_branch, else_branch) ->
        check_ite_expression ctx loc cond then_branch else_branch
    | Aexpr_call (callee, args) -> check_call_expression ctx loc callee args
    | Aexpr_every (call, cond, every_loc) ->
        check_every_expression ctx loc call cond every_loc
    | Aexpr_merge (cond, branches, merge_loc) ->
        check_merge_expression ctx loc cond branches merge_loc
    | Aexpr_when (expr, constructor, cond, when_loc) ->
        check_when_expression ctx loc expr constructor cond when_loc
    | Aexpr_when_bool (expr, cond, when_loc) ->
        check_when_bool_expression ctx loc expr cond when_loc
    | Aexpr_seq (first, second) -> check_seq_expression ctx loc first second
    | Aexpr_match (expr, branches) ->
        check_match_expression ctx loc expr branches
  with Error (msg, loc, notes) ->
    had_error := true;
    if not !in_autocomplete_mode then Error.print_error msg loc notes;
    mk_texpr Texpr_error Ttype_bottom Tclock_static main_expr.loc

and check_unit_expression loc =
  mk_texpr Texpr_unit Type.unit_type Tclock_static loc

and check_integer_expression loc value =
  let typ = Ttype_int in
  (if not (Z.fits_int value) then
     let msg =
       Format.asprintf
         "Integer literal exceeds the range of representable integers of type \
          %a."
         Tast_printer.pp_type typ
     in
     error msg loc);

  mk_texpr (Texpr_int value) typ Tclock_static loc

and check_float_expression loc value =
  mk_texpr (Texpr_float value) Ttype_float Tclock_static loc

and check_string_expression loc value =
  mk_texpr (Texpr_string value) Ttype_string Tclock_static loc

and check_char_expression loc value =
  mk_texpr (Texpr_char value) Ttype_char Tclock_static loc

and check_tuple_expression ctx loc elements =
  let telements = List.map (check_expression ctx) elements in
  let element_types = List.map (fun te -> te.texpr_type) telements in
  let element_clocks = List.map (fun te -> te.texpr_clock) telements in
  let tuple_type = Ttype_tuple element_types in
  let tuple_clock = Tclock_tuple element_clocks in
  mk_texpr (Texpr_tuple telements) tuple_type tuple_clock loc

and check_paren_expression ctx loc e =
  let te = check_expression ctx e in
  (* We keep parenthesis if needed for diagnostics, pretty printing, etc. *)
  mk_texpr (Texpr_paren te) te.texpr_type te.texpr_clock loc

and check_variable_expression ctx loc id =
  let decl =
    lookup
      ~error_msg:(fun symbol ->
        (Format.asprintf "Unbound identifier '%s'." symbol.value, symbol.loc))
      ctx.ctx_decls_table id
  in

  match decl with
  | Tdecl_function func_decl ->
      let func_type, type_subst =
        Type.instantiate ctx.ctx_current_level func_decl.tdecl_fun_type
      in
      let func_clock =
        Clock.instantiate ctx.ctx_current_level func_decl.tdecl_fun_clock
      in

      (* Prints warning if deprecated *)
      deprecated_usage "Function" id func_decl.tdecl_fun_deprecated;

      mk_texpr (Texpr_func_ref (func_decl, type_subst)) func_type func_clock loc
  | _ ->
      let typ = type_of_decl decl in
      let clock = clock_of_decl decl in
      mk_texpr (Texpr_ref decl) typ clock loc

and check_let_expression ctx loc bindings body rec_loc =
  (* TODO: correctly handle recursive let bindings *)
  let is_recursive = Option.is_some rec_loc in
  ignore is_recursive;

  let added_symbols, let_declarations =
    List.fold_left_map
      (fun symbol_table (p, value) ->
        match p.value with
        | Apat_ident id ->
            let let_type = Type.fresh_var ctx.ctx_current_level in
            let let_clock = Clock.fresh_var ctx.ctx_current_level in
            let let_declaration =
              {
                tdecl_let_name = id;
                tdecl_let_type = let_type;
                tdecl_let_clock = let_clock;
                tdecl_let_value = None;
              }
            in

            let name = let_declaration.tdecl_let_name.value in
            let new_symbol_table =
              match StringMap.find_opt name symbol_table with
              | Some _ ->
                  let msg =
                    Format.asprintf "Duplicate let binding for '%s'." name
                  in
                  error msg let_declaration.tdecl_let_name.loc
              | None ->
                  StringMap.add name (Tdecl_let let_declaration) symbol_table
            in
            (new_symbol_table, (let_declaration, value))
        | _ -> failwith "Pattern matching in let bindings is not yet supported.")
      StringMap.empty bindings
  in

  let new_context =
    {
      ctx with
      ctx_decls_table =
        StringMap.union
          (fun _ new_symbol _ -> Some new_symbol)
          added_symbols ctx.ctx_decls_table;
    }
  in

  let let_declarations =
    List.map
      (fun (decl, value_expr) ->
        let tvalue_expr = check_expression new_context value_expr in
        (try Type.unify decl.tdecl_let_type tvalue_expr.texpr_type
         with Type.UnificationError _ ->
           (* The only possible way to have an unification error here is
             because we are trying to unify mutually recursive types (one
             type variable occurs in the other type).

             And this is only possible if we have a cycle in the let bindings,
             which should be diagnosed by the causality checker. Because the
             later is able to produce much better errors for cycles, we ignore
             the unification error and let the causality checker emit the error later. *)
           ());

        Clock.unify decl.tdecl_let_clock tvalue_expr.texpr_clock;
        decl.tdecl_let_value <- Some tvalue_expr;
        decl)
      let_declarations
  in

  let tbody = check_expression new_context body in
  mk_texpr
    (Texpr_let { bindings = let_declarations; body = tbody; rec_loc })
    tbody.texpr_type tbody.texpr_clock loc

and check_binary_expression ctx loc op lhs rhs =
  match op.value with
  | Abinop_add | Abinop_sub | Abinop_mul | Abinop_div | Abinop_mod ->
      let tlhs = check_expression ctx lhs in
      let trhs = check_expression ctx rhs in
      unify_expressions tlhs trhs;
      expect_type tlhs Ttype_int;
      let clock = common_clock tlhs.texpr_clock trhs.texpr_clock in
      mk_texpr (Texpr_binary (op, tlhs, trhs)) tlhs.texpr_type clock loc
  | Abinop_fadd | Abinop_fsub | Abinop_fmul | Abinop_fdiv ->
      let tlhs = check_expression ctx lhs in
      let trhs = check_expression ctx rhs in
      unify_expressions tlhs trhs;
      expect_type tlhs Ttype_float;
      let clock = common_clock tlhs.texpr_clock trhs.texpr_clock in
      mk_texpr (Texpr_binary (op, tlhs, trhs)) tlhs.texpr_type clock loc
  | Abinop_logand | Abinop_logor ->
      let tlhs = check_expression ctx lhs in
      let trhs = check_expression ctx rhs in
      unify_expressions tlhs trhs;
      expect_boolean_expr ctx tlhs;
      let clock = common_clock tlhs.texpr_clock trhs.texpr_clock in
      mk_texpr (Texpr_binary (op, tlhs, trhs)) tlhs.texpr_type clock loc
  | Abinop_eq | Abinop_ne | Abinop_lt | Abinop_le | Abinop_gt | Abinop_ge ->
      let tlhs = check_expression ctx lhs in
      let trhs = check_expression ctx rhs in
      unify_expressions tlhs trhs;
      let clock = common_clock tlhs.texpr_clock trhs.texpr_clock in
      let result_type = Type.bool_type in
      mk_texpr (Texpr_binary (op, tlhs, trhs)) result_type clock loc
  | Abinop_init | Abinop_fby ->
      let tlhs = check_expression ctx lhs in
      let trhs = check_expression ctx rhs in
      unify_types_in_expressions tlhs trhs;
      expect_constant tlhs;

      mk_texpr
        (Texpr_binary (op, tlhs, trhs))
        tlhs.texpr_type trhs.texpr_clock loc

and check_unary_expression ctx loc op expr =
  let texpr = check_expression ctx expr in
  match op.value with
  | Aunop_neg ->
      expect_type texpr Ttype_int;
      mk_texpr (Texpr_unary (op, texpr)) texpr.texpr_type texpr.texpr_clock loc
  | Aunop_fneg ->
      expect_type texpr Ttype_float;
      mk_texpr (Texpr_unary (op, texpr)) texpr.texpr_type texpr.texpr_clock loc
  | Aunop_not ->
      expect_boolean_expr ctx texpr;
      mk_texpr (Texpr_unary (op, texpr)) texpr.texpr_type texpr.texpr_clock loc
  | Aunop_pre | Aunop_last ->
      mk_texpr (Texpr_unary (op, texpr)) texpr.texpr_type texpr.texpr_clock loc

and check_ite_expression ctx loc cond then_branch else_branch =
  let tcond = check_expression ctx cond in
  let tthen = check_expression ctx then_branch in
  let telse = check_expression ctx else_branch in
  expect_boolean_expr ctx tcond;
  unify_expressions tthen telse;

  mk_texpr
    (Texpr_ite (tcond, tthen, telse))
    tthen.texpr_type tthen.texpr_clock loc

and check_call_expression ctx loc callee args =
  let tcallee = check_expression ctx callee in
  let targs = List.map (check_expression ctx) args in

  (* Create the expected function type and clock. *)
  let return_type = Type.fresh_var ctx.ctx_current_level in
  let return_clock = Clock.fresh_var ctx.ctx_current_level in
  let param_types =
    List.init (List.length targs) (fun _ ->
        Type.fresh_var ctx.ctx_current_level)
  in
  let param_clocks =
    List.init (List.length targs) (fun _ ->
        Clock.fresh_var ctx.ctx_current_level)
  in
  let expected_type =
    match Type.prune tcallee.texpr_type with
    | Ttype_node _ -> Ttype_node (param_types, return_type)
    | _ -> Ttype_function (param_types, return_type)
  in

  let expected_clock = Tclock_function (param_clocks, return_clock) in

  (* Check arguments for types *)
  List.iter2
    (fun arg_texpr param_type ->
      try Type.unify arg_texpr.texpr_type param_type
      with Type.UnificationError _ ->
        let msg =
          Format.asprintf
            "Cannot unify argument type '%a' with parameter type '%a'."
            Tast_printer.pp_type arg_texpr.texpr_type Tast_printer.pp_type
            param_type
        in
        error msg arg_texpr.texpr_loc)
    targs param_types;

  (* Check arguments for clocks *)
  List.iter2
    (fun arg_texpr param_clock ->
      try Clock.unify arg_texpr.texpr_clock param_clock
      with Clock.UnificationError _ ->
        let msg =
          Format.asprintf
            "Cannot unify argument clock '%a' with parameter clock '%a'."
            Tast_printer.pp_clock arg_texpr.texpr_clock Tast_printer.pp_clock
            param_clock
        in
        error msg arg_texpr.texpr_loc)
    targs param_clocks;

  (* Unify callee with expected function type and clock *)
  (try Type.unify tcallee.texpr_type expected_type
   with Type.UnificationError reason -> (
     match reason with
     | TooFewArguments (_, expected_args, given_args) ->
         let msg =
           Format.asprintf "Too few arguments; expected %d, but got %d."
             expected_args given_args
         in
         error msg tcallee.texpr_loc
     | TooManyArguments (_, expected_args, given_args) ->
         let msg =
           Format.asprintf "Too many arguments; expected %d, but got %d."
             expected_args given_args
         in
         error msg tcallee.texpr_loc
     | _ ->
         let msg =
           Format.asprintf "Expected function type '%a', but got type '%a'."
             Tast_printer.pp_type expected_type Tast_printer.pp_type
             tcallee.texpr_type
         in
         error msg tcallee.texpr_loc));

  (try Clock.unify tcallee.texpr_clock expected_clock
   with Clock.UnificationError _ ->
     (* No reason to emit a special error about mismatched parameters
        as it was handled by the Type unification. *)
     let msg =
       Format.asprintf "Expected function clock '%a', but got clock '%a'."
         Tast_printer.pp_clock expected_clock Tast_printer.pp_clock
         tcallee.texpr_clock
     in
     error msg tcallee.texpr_loc);

  mk_texpr (Texpr_call (tcallee, targs)) return_type return_clock loc

and check_every_expression ctx loc call cond every_loc =
  let tcall = check_expression ctx call in
  let tcond = check_expression ctx cond in
  expect_boolean_expr ctx tcond;

  let callee, _ =
    match (ignore_parens tcall).texpr_kind with
    | Texpr_call (callee, args) -> (callee, args)
    | _ -> error "Expected a node call in 'every' expression." tcall.texpr_loc
  in

  let node_declaration = extract_function_declaration callee in
  (if not node_declaration.tdecl_fun_is_node then
     let msg =
       "Expected a node call in 'every' expression, but got a function."
     in
     let note_msg =
       Format.asprintf "Function '%s' declared here."
         node_declaration.tdecl_fun_name.value
     in
     error_with_note ~ignore_note_if_dummy:true msg callee.texpr_loc note_msg
       node_declaration.tdecl_fun_name.loc);

  mk_texpr
    (Texpr_every (tcall, tcond, every_loc))
    tcall.texpr_type tcall.texpr_clock loc

(** Type checks all [branches] and returns the common enum declaration to all
    branches conditions. *)
and check_match_branches expr_name ctx branches =
  assert (branches <> []);
  let enum_decl = ref None in
  let head_branch_expression = ref None in
  let tbranches =
    List.map
      (fun (pattern, expr) ->
        let id =
          match pattern.value with
          | Apat_ident id -> id
          | _ ->
              let msg =
                Format.asprintf
                  "Only identifier patterns are supported in %s expressions."
                  expr_name
              in
              error msg pattern.loc
        in

        let constructor = lookup_constructor ctx id in

        (match !enum_decl with
        | None -> enum_decl := Some constructor.tdecl_constructor_enum
        | Some enum_decl ->
            if constructor.tdecl_constructor_enum != enum_decl then
              let msg =
                Format.asprintf
                  "Constructor '%s' does not belong to enum type '%s'."
                  constructor.tdecl_constructor_name.value
                  enum_decl.tdecl_enum_name.value
              in
              let note_msg =
                Format.asprintf "Enum type '%s' declared here."
                  enum_decl.tdecl_enum_name.value
              in
              error_with_note ~ignore_note_if_dummy:true msg id.loc note_msg
                enum_decl.tdecl_enum_name.loc);

        let texpr = check_expression ctx expr in
        (match !head_branch_expression with
        | None -> head_branch_expression := Some texpr
        | Some head_branch_expr ->
            unify_types_in_expressions head_branch_expr texpr);

        (constructor, texpr))
      branches
  in
  (* The Option.get is safe here because we always have at least one branch
     and therefore, we set enum_decl at least once. *)
  (tbranches, Option.get !enum_decl)

(** Checks that [branches] are exhaustive for [enum_decl]. *)
and check_match_branches_exhaustive expr_name loc enum_decl branches =
  (* Retrieve constructors names of [enum_decl]. *)
  let declared_constructors =
    List.map fst branches
    |> List.map (fun ctor -> ctor.tdecl_constructor_name.value)
  in
  (* Then check if there is any missing branch (an unmatched constructor). *)
  List.iter
    (fun ctor ->
      if not (List.mem ctor.tdecl_constructor_name.value declared_constructors)
      then
        let msg =
          Format.asprintf
            "Missing branch for constructor '%s' in %s expression."
            ctor.tdecl_constructor_name.value expr_name
        in
        let note_msg =
          Format.asprintf "Constructor '%s' declared here."
            ctor.tdecl_constructor_name.value
        in
        error_with_note ~ignore_note_if_dummy:true msg loc note_msg
          ctor.tdecl_constructor_name.loc)
    enum_decl.tdecl_enum_constructors

and check_match_base_expression expr_name ctx loc cond branches =
  let tcond = check_expression ctx cond in
  let tbranches, enum_decl = check_match_branches expr_name ctx branches in

  check_match_branches_exhaustive expr_name loc enum_decl tbranches;
  (tcond, tbranches)

and check_merge_expression ctx loc cond branches merge_loc =
  let tcond, tbranches =
    check_match_base_expression "merge" ctx loc cond branches
  in

  (* Check if all branches have the expected clock. *)
  List.iter
    (fun (ctor, texpr) ->
      let expected_clock = Tclock_on (tcond.texpr_clock, ctor, tcond) in
      try Clock.unify expected_clock texpr.texpr_clock
      with Clock.UnificationError _ ->
        let msg =
          Format.asprintf
            "Expected clock '%a' on branch for constructor '%s', but got clock \
             '%a'."
            Tast_printer.pp_clock expected_clock
            ctor.tdecl_constructor_name.value Tast_printer.pp_clock
            texpr.texpr_clock
        in
        error msg texpr.texpr_loc)
    tbranches;

  let head_branch_expr = snd (List.hd tbranches) in
  mk_texpr
    (Texpr_merge (tcond, tbranches, merge_loc))
    head_branch_expr.texpr_type tcond.texpr_clock loc

and check_match_expression ctx loc cond branches =
  let tcond, tbranches =
    check_match_base_expression "match" ctx loc cond branches
  in
  ignore tcond;
  ignore tbranches;
  error "Pattern matching is not yet implemented." loc

and check_when_expression ctx loc body constructor cond when_loc =
  let tbody = check_expression ctx body in
  let tcond = check_expression ctx cond in
  unify_clocks_in_expressions tbody tcond;

  let constructor = lookup_constructor ctx constructor in
  expect_type tcond (Ttype_enum constructor.tdecl_constructor_enum);

  let clock = Tclock_on (tbody.texpr_clock, constructor, tcond) in
  mk_texpr
    (Texpr_when (tbody, constructor, tcond, when_loc))
    tbody.texpr_type clock loc

and check_when_bool_expression ctx loc body cond when_loc =
  let tbody = check_expression ctx body in
  let tcond = check_expression ctx cond in
  unify_clocks_in_expressions tbody tcond;
  expect_boolean_expr ctx tcond;

  let clock = Tclock_on (tbody.texpr_clock, Type.true_constructor, tcond) in
  mk_texpr
    (Texpr_when (tbody, Type.true_constructor, tcond, when_loc))
    tbody.texpr_type clock loc

and check_seq_expression ctx loc first second =
  let tfirst = check_expression ctx first in
  let tsecond = check_expression ctx second in

  (if not (Type.is_unit tfirst.texpr_type) then
     let msg =
       Format.asprintf
         "The left-hand side of a sequence should be of type unit, but it is \
          of type %a."
         Tast_printer.pp_type tfirst.texpr_type
     in
     warning msg tfirst.texpr_loc);

  mk_texpr
    (Texpr_seq (tfirst, tsecond))
    tsecond.texpr_type tsecond.texpr_clock loc
