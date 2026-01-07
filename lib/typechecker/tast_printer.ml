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

(* This file contains functions to pretty-print TAST structures for debugging
   and diagnostics purposes. *)

open Tree_printer
open Tast
open Printer_utils
open Format

type tree_node =
  | TN_program
  | TN_expr of string * texpression * (formatter -> unit)
  | TN_decl of
      string * Location.t * string * Type.t * Clock.t * (formatter -> unit)
  | TN_merge_branch of constructor_declaration

let type_var_mapping_scopes =
  let stack = Stack.create () in
  Stack.push (Hashtbl.create 0, 0) stack;
  stack

let clock_var_mapping_scopes =
  let stack = Stack.create () in
  Stack.push (Hashtbl.create 0, 0) stack;
  stack

let enter_type_var_context () =
  let prev_type_var_mapping, type_var_counter =
    Stack.top type_var_mapping_scopes
  in
  Stack.push
    (Hashtbl.create 0, type_var_counter + Hashtbl.length prev_type_var_mapping)
    type_var_mapping_scopes;

  let prev_clock_var_mapping, clock_var_counter =
    Stack.top clock_var_mapping_scopes
  in
  Stack.push
    (Hashtbl.create 0, clock_var_counter + Hashtbl.length prev_clock_var_mapping)
    clock_var_mapping_scopes

let exit_type_var_context () =
  Stack.drop type_var_mapping_scopes;
  Stack.drop clock_var_mapping_scopes

exception Found of string

let lookup_type_var stack v =
  try
    Stack.iter
      (fun (mapping, _) ->
        match Hashtbl.find_opt mapping v with
        | Some name -> raise (Found name)
        | None -> ())
      stack;
    None
  with Found name -> Some name

(** Pretty prints the provided source location to the formatter. *)
let pp_location =
  let aux fmt (b, e) =
    if b = Lexing.dummy_pos || e = Lexing.dummy_pos then
      pp_print_string fmt "<unknown>"
    else
      let start_line = b.pos_lnum in
      let end_line = e.pos_lnum in
      let start_column = b.pos_cnum - b.pos_bol + 1 in
      let end_column = e.pos_cnum - e.pos_bol + 1 in
      if start_line = end_line then
        fprintf fmt "<%d:%d-%d>" start_line start_column end_column
      else
        fprintf fmt "<%d:%d-%d:%d>" start_line start_column end_line end_column
  in
  pp_with_color Location aux

let pp_constructor =
  pp_with_color Constructor (fun fmt ctor ->
      fprintf fmt "%s" ctor.tdecl_constructor_name.value)

(** Pretty prints the provided type [t] to the formatter. *)
let pp_type fmt t =
  let pp_list pp sep =
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " %s@ " sep) pp
  in

  (* Return true if this type is atomic (no parentheses needed when nested). *)
  let is_atomic t =
    match Type.prune t with
    | Ttype_named _ | Ttype_bottom | Ttype_int | Ttype_float | Ttype_string
    | Ttype_char | Ttype_var _ | Ttype_enum _ | Ttype_paren _ ->
        true
    | Ttype_tuple _ | Ttype_function _ | Ttype_node _ -> false
  in

  let rec aux ~top fmt t =
    let t = Type.prune ~keep_parens:true t in
    let needs_paren = (not top) && not (is_atomic t) in

    if needs_paren then fprintf fmt "(";

    (match t with
    | Ttype_named (name, _) -> pp_with_color Type pp_print_string fmt name.value
    | Ttype_paren t -> fprintf fmt "(@[<hov>%a@])" (aux ~top:false) t
    | Ttype_bottom -> pp_string_with_color Error fmt "⊥"
    | Ttype_int -> pp_keyword fmt "int"
    | Ttype_float -> pp_keyword fmt "float"
    | Ttype_string -> pp_keyword fmt "string"
    | Ttype_char -> pp_keyword fmt "char"
    | Ttype_enum enum_decl when enum_decl == Type.bool_declaration ->
        pp_keyword fmt "bool"
    | Ttype_enum enum_decl ->
        pp_with_color Type pp_print_string fmt enum_decl.tdecl_enum_name.value
    | Ttype_function ([], ret) -> fprintf fmt "() -> %a" (aux ~top:false) ret
    | Ttype_function (params, ret) ->
        fprintf fmt "@[<hov2>%a@ -> %a@]"
          (pp_list (aux ~top:false) "->")
          params (aux ~top:false) ret
    | Ttype_node ([], ret) -> fprintf fmt "() => %a" (aux ~top:false) ret
    | Ttype_node (params, ret) ->
        fprintf fmt "@[<hov2>%a@ => %a@]"
          (pp_list (aux ~top:false) "->")
          params (aux ~top:false) ret
    | Ttype_tuple types ->
        fprintf fmt "@[<hov2>%a@]" (pp_list (aux ~top:false) "*") types
    | Ttype_var v when !print_var_types_ids ->
        pp_with_color TypeVariable
          (fun fmt v -> fprintf fmt "'a%d" v.ttype_var_id)
          fmt v
    | Ttype_var v -> (
        let pp_type_var fmt name =
          pp_with_color TypeVariable
            (fun fmt name -> fprintf fmt "'%s" name)
            fmt name
        in
        match lookup_type_var type_var_mapping_scopes v with
        | Some name -> pp_type_var fmt name
        | None ->
            (* Find a fresh name for the quantified variable, starting with lowercase letters,
               then adding numbers if needed. *)
            let compute_name n =
              let base = Char.chr (Char.code 'a' + (n mod 26)) in
              if n < 26 then String.make 1 base
              else Printf.sprintf "%c%d" base (n / 26)
            in
            let type_var_mapping, counter = Stack.top type_var_mapping_scopes in
            let name =
              compute_name (counter + Hashtbl.length type_var_mapping)
            in
            pp_type_var fmt name;
            Hashtbl.add type_var_mapping v name));

    if needs_paren then fprintf fmt ")"
  in
  aux ~top:true fmt t

let pp_binop fmt op =
  let open Ast in
  match op.value with
  | Abinop_add -> pp_print_string fmt "+"
  | Abinop_sub -> pp_print_string fmt "-"
  | Abinop_mul -> pp_print_string fmt "*"
  | Abinop_div -> pp_print_string fmt "/"
  | Abinop_mod -> pp_print_string fmt "mod"
  | Abinop_fadd -> pp_print_string fmt "+."
  | Abinop_fsub -> pp_print_string fmt "-."
  | Abinop_fmul -> pp_print_string fmt "*."
  | Abinop_fdiv -> pp_print_string fmt "/."
  | Abinop_logand -> pp_keyword fmt "&&"
  | Abinop_logor -> pp_keyword fmt "||"
  | Abinop_eq -> pp_print_string fmt "=="
  | Abinop_ne -> pp_print_string fmt "!="
  | Abinop_lt -> pp_print_string fmt "<"
  | Abinop_le -> pp_print_string fmt "<="
  | Abinop_gt -> pp_print_string fmt ">"
  | Abinop_ge -> pp_print_string fmt ">="
  | Abinop_init -> pp_print_string fmt "->"
  | Abinop_fby -> pp_keyword fmt "fby"

let pp_unop fmt op =
  let open Ast in
  match op.value with
  | Aunop_neg -> pp_print_string fmt "-"
  | Aunop_fneg -> pp_print_string fmt "-."
  | Aunop_not -> pp_keyword fmt "not"
  | Aunop_pre -> pp_keyword fmt "pre"
  | Aunop_last -> pp_keyword fmt "last"

let rec pp_expr fmt expr =
  match expr.texpr_kind with
  | Texpr_error -> pp_string_with_color Error fmt "<error>"
  | Texpr_unit -> pp_print_string fmt "()"
  | Texpr_int n -> pp_int fmt n
  | Texpr_float n -> pp_float fmt n
  | Texpr_string s -> pp_string fmt s
  | Texpr_char c -> pp_char fmt c
  | Texpr_tuple exprs ->
      fprintf fmt "@[<hov2>(";
      pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_expr fmt exprs;
      fprintf fmt ")@]"
  | Texpr_ref decl -> fprintf fmt "%s" (Tast_utils.name_of_decl decl)
  | Texpr_func_ref (func_decl, _type_args) ->
      fprintf fmt "%s" func_decl.tdecl_fun_name.value
  | Texpr_paren e -> fprintf fmt "(@[<hov>%a@])" pp_expr e
  | Texpr_seq (first, second) ->
      fprintf fmt "@[<hov>%a;@ %a@]" pp_expr first pp_expr second
  | Texpr_let { bindings; body; _ } ->
      fprintf fmt "@[<hov2>let@ ";
      List.iter
        (fun let_decl ->
          let name = let_decl.tdecl_let_name.value in
          fprintf fmt "%s = " name;
          match let_decl.tdecl_let_value with
          | Some expr -> fprintf fmt "%a;@ " pp_expr expr
          | None -> fprintf fmt "<uninitialized>;@ ")
        bindings;
      fprintf fmt "in@ %a@]" pp_expr body
  | Texpr_ite (cond, if_true, if_false) ->
      fprintf fmt "@[<hov2>if@ %a@ then@ %a@ else@ %a@]" pp_expr cond pp_expr
        if_true pp_expr if_false
  | Texpr_binary (op, lhs, rhs) ->
      fprintf fmt "@[<hov2>%a %a %a@]" pp_expr lhs pp_binop op pp_expr rhs
  | Texpr_unary (op, expr) ->
      fprintf fmt "@[<hov2>%a %a@]" pp_unop op pp_expr expr
  | Texpr_call (callee, args) ->
      fprintf fmt "@[<hov2>%a(@ " pp_expr callee;
      pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_expr fmt args;
      fprintf fmt ")@]"
  | Texpr_merge (cond, branches, _) ->
      fprintf fmt "@[<hov2>merge@ %a@ with@ " pp_expr cond;
      List.iter
        (fun (ctor, branch_expr) ->
          fprintf fmt "| %a -> %a@ " pp_constructor ctor pp_expr branch_expr)
        branches;
      fprintf fmt "@]"
  | Texpr_when _ -> fprintf fmt "?"
  | Texpr_every (call, cond, _) ->
      fprintf fmt "@[<hov2>every@ %a@ on@ %a@]" pp_expr call pp_expr cond

let pp_clock fmt c =
  let pp_list pp sep =
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " %s@ " sep) pp
  in

  (* Return true if this clock type is atomic (no parentheses needed when nested). *)
  let is_atomic t =
    match Clock.prune t with
    | Tclock_var _ | Tclock_static | Tclock_paren _ | Tclock_named _ -> true
    | Tclock_tuple _ | Tclock_function _ | Tclock_on _ -> false
  in

  let rec aux ~top fmt t =
    let t = Clock.prune ~keep_parens:true t in
    let needs_paren = (not top) && not (is_atomic t) in

    if needs_paren then fprintf fmt "(";

    (match t with
    | Tclock_named (name, _) ->
        pp_with_color Type pp_print_string fmt name.value
    | Tclock_paren t -> fprintf fmt "(@[<hov>%a@])" (aux ~top:false) t
    | Tclock_static -> pp_keyword fmt "static"
    | Tclock_function ([], ret) -> fprintf fmt "() -> %a" (aux ~top:false) ret
    | Tclock_function (params, ret) ->
        fprintf fmt "@[<hov2>%a@ -> %a@]"
          (pp_list (aux ~top:false) "->")
          params (aux ~top:false) ret
    | Tclock_tuple types ->
        fprintf fmt "@[<hov2>%a@]" (pp_list (aux ~top:false) "*") types
    | Tclock_on (base, ctor, expr) ->
        fprintf fmt "@[<hov2>%a %a %a(%a)@]" (aux ~top:false) base pp_keyword
          "on" pp_constructor ctor pp_expr expr
    | Tclock_var v when !print_var_types_ids ->
        pp_with_color TypeVariable
          (fun fmt v -> fprintf fmt "'a%d" v.tclock_var_id)
          fmt v
    | Tclock_var v -> (
        let pp_type_var fmt name =
          pp_with_color TypeVariable
            (fun fmt name -> fprintf fmt "'%s" name)
            fmt name
        in
        match lookup_type_var clock_var_mapping_scopes v with
        | Some name -> pp_type_var fmt name
        | None ->
            (* Find a fresh name for the quantified variable, starting with lowercase letters,
               then adding numbers if needed. *)
            let compute_name n =
              let base = Char.chr (Char.code 'a' + (n mod 26)) in
              if n < 26 then String.make 1 base
              else Printf.sprintf "%c%d" base (n / 26)
            in
            let clock_var_mapping, counter =
              Stack.top clock_var_mapping_scopes
            in
            let name =
              compute_name (counter + Hashtbl.length clock_var_mapping)
            in
            pp_type_var fmt name;
            Hashtbl.add clock_var_mapping v name));

    if needs_paren then fprintf fmt ")"
  in
  aux ~top:true fmt c

let print_tree_node fmt = function
  | TN_program -> pp_string_with_color LabelDecl fmt "Program"
  | TN_expr (label, expr, pp_extra) ->
      pp_string_with_color LabelExpr fmt label;
      fprintf fmt " ";
      if !print_locations then fprintf fmt "%a " pp_location expr.texpr_loc;
      if !print_types then fprintf fmt "{%a} " pp_type expr.texpr_type;
      if !print_clocks then fprintf fmt "<%a> " pp_clock expr.texpr_clock;
      pp_extra fmt
  | TN_decl (label, location, name, typ, clock, pp_extra) ->
      enter_type_var_context ();
      pp_string_with_color LabelDecl fmt label;
      fprintf fmt " ";
      if !print_locations then fprintf fmt "%a " pp_location location;
      fprintf fmt "%s " name;
      if !print_types then fprintf fmt "{%a} " pp_type typ;
      if !print_clocks then fprintf fmt "<%a> " pp_clock clock;
      pp_extra fmt
  | TN_merge_branch ctor_decl ->
      pp_with_color LabelDecl
        (fun fmt ctor -> fprintf fmt "Branch %a" pp_constructor ctor)
        fmt ctor_decl

let print_tree_node_cleanup = function
  | TN_program -> ()
  | TN_expr (_, _, _) -> ()
  | TN_decl (_, _, _, _, _, _) -> exit_type_var_context ()
  | TN_merge_branch _ -> ()

let decl_tree_node decl typ clock =
  let decl_label =
    match decl with
    | Tdecl_global _ -> "GlobalDecl"
    | Tdecl_function func_decl when func_decl.tdecl_fun_is_node -> "NodeDecl"
    | Tdecl_function _ -> "FunctionDecl"
    | Tdecl_param _ -> "ParamDecl"
    | Tdecl_let _ -> "LetDecl"
    | Tdecl_constructor _ -> "ConstructorDecl"
  in
  let location = Tast_utils.location_of_decl decl in
  let name = Tast_utils.name_of_decl decl in
  TN_decl (decl_label, location, name, typ, clock, fun _ -> ())

let rec expr_tree expr =
  match expr.texpr_kind with
  | Texpr_error -> Node (TN_expr ("ExprError", expr, fun _ -> ()), [])
  | Texpr_unit -> Node (TN_expr ("ExprUnit", expr, fun _ -> ()), [])
  | Texpr_int n -> Node (TN_expr ("ExprInt", expr, fun fmt -> pp_int fmt n), [])
  | Texpr_float n ->
      Node (TN_expr ("ExprFloat", expr, fun fmt -> pp_float fmt n), [])
  | Texpr_string s ->
      Node (TN_expr ("ExprString", expr, fun fmt -> pp_string fmt s), [])
  | Texpr_char c ->
      Node (TN_expr ("ExprChar", expr, fun fmt -> pp_char fmt c), [])
  | Texpr_tuple exprs ->
      Node (TN_expr ("ExprTuple", expr, fun _ -> ()), List.map expr_tree exprs)
  | Texpr_ref decl ->
      Node
        ( TN_expr ("ExprDeclRef", expr, fun _ -> ()),
          [ Node (decl_tree_node decl expr.texpr_type expr.texpr_clock, []) ] )
  | Texpr_func_ref (func_decl, _type_args) ->
      Node
        ( TN_expr ("ExprFuncRef", expr, fun _ -> ()),
          [
            Node
              ( decl_tree_node (Tdecl_function func_decl) expr.texpr_type
                  expr.texpr_clock,
                [] );
          ] )
  | Texpr_paren e ->
      Node (TN_expr ("ExprParen", expr, fun _ -> ()), [ expr_tree e ])
  | Texpr_seq (first, second) ->
      Node
        ( TN_expr ("ExprSeq", expr, fun _ -> ()),
          [ expr_tree first; expr_tree second ] )
  | Texpr_let { bindings; body; _ } ->
      let binding_trees =
        List.map
          (fun let_decl ->
            let loc = let_decl.tdecl_let_name.loc in
            let name = let_decl.tdecl_let_name.value in
            let typ = let_decl.tdecl_let_type in
            let clock = let_decl.tdecl_let_clock in
            Node
              ( TN_decl ("LetBinding", loc, name, typ, clock, fun _ -> ()),
                match let_decl.tdecl_let_value with
                | Some expr -> [ expr_tree expr ]
                | None -> [] ))
          bindings
      in
      Node
        ( TN_expr ("ExprLet", expr, fun _ -> ()),
          binding_trees @ [ expr_tree body ] )
  | Texpr_ite (cond, if_true, if_false) ->
      Node
        ( TN_expr ("ExprIte", expr, fun _ -> ()),
          [ expr_tree cond; expr_tree if_true; expr_tree if_false ] )
  | Texpr_binary (op, lhs, rhs) ->
      Node
        ( TN_expr ("ExprBinary", expr, fun fmt -> fprintf fmt "%a" pp_binop op),
          [ expr_tree lhs; expr_tree rhs ] )
  | Texpr_unary (op, expr) ->
      Node
        ( TN_expr ("ExprUnary", expr, fun fmt -> fprintf fmt "%a" pp_unop op),
          [ expr_tree expr ] )
  | Texpr_call (callee, args) ->
      Node
        ( TN_expr ("ExprCall", expr, fun _ -> ()),
          expr_tree callee :: List.map expr_tree args )
  | Texpr_merge (cond, branches, _) ->
      let branch_trees =
        List.map
          (fun (ctor, branch_expr) ->
            Node (TN_merge_branch ctor, [ expr_tree branch_expr ]))
          branches
      in
      Node
        ( TN_expr ("ExprMerge", expr, fun _ -> ()),
          expr_tree cond :: branch_trees )
  | Texpr_when (body, constructor, cond, _when_loc) ->
      Node
        ( TN_expr ("ExprWhen", expr, fun fmt -> pp_constructor fmt constructor),
          [ expr_tree body; expr_tree cond ] )
  | Texpr_every (call, cond, _) ->
      Node
        ( TN_expr ("ExprEvery", expr, fun _ -> ()),
          [ expr_tree call; expr_tree cond ] )

let node_tree node_decl =
  let params_tree =
    List.map
      (fun param ->
        Node
          ( decl_tree_node (Tdecl_param param) param.tdecl_param_type
              param.tdecl_param_clock,
            [] ))
      node_decl.tdecl_fun_params
  in

  let tree_body = expr_tree node_decl.tdecl_fun_body in

  Node
    ( decl_tree_node (Tdecl_function node_decl) node_decl.tdecl_fun_type
        node_decl.tdecl_fun_clock,
      params_tree @ [ tree_body ] )

let pp_program fmt declarations =
  let decl_trees =
    List.map
      (fun decl ->
        let tree =
          match decl with
          | Tdecl_function fdecl -> node_tree fdecl
          | _ -> failwith "TODO"
        in
        tree)
      declarations
  in
  print_tree print_tree_node print_tree_node_cleanup fmt
    (Node (TN_program, decl_trees))
