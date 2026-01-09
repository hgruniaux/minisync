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

open Typechecker_common

type item_type =
  | ACitem_type
  | ACitem_function
  | ACitem_variable
  | ACitem_constructor
  | ACitem_enum
  | ACitem_struct
  | ACitem_field

type autocomplete_item = {
  ac_item_kind : item_type;
  ac_item_label : string;
  ac_item_loc : Location.t;
}

exception Autocomplete of autocomplete_item list

type autocomplete_expr_request = {
  ac_request_decls : Tast.declaration_kind StringMap.t;
  ac_request_prefix : string option;
  ac_request_suffix : string option;
  ac_request_type : Tast.ttype option;
  ac_request_loc : Location.t;
}

let request = ref None

let filter_and_sort suggestions prefix suffix =
  ignore suffix;
  let filtered =
    match prefix with
    | None -> suggestions
    | Some pfx ->
        List.filter
          (fun item ->
            let name = item.ac_item_label in
            String.starts_with ~prefix:pfx name)
          suggestions
  in
  filtered

let autocomplete_constructor enum_decl loc prefix suffix =
  ignore suffix;
  let suggestions =
    List.map
      (fun ctor_decl ->
        let name = ctor_decl.Tast.tdecl_constructor_name.value in
        let suggestion =
          {
            ac_item_kind = ACitem_constructor;
            ac_item_label = name;
            ac_item_loc = loc;
          }
        in
        suggestion)
      enum_decl.Tast.tdecl_enum_constructors
  in
  raise (Autocomplete (filter_and_sort suggestions prefix suffix))

let autocomplete_expr ctx loc prefix suffix =
  (* We differ the autocompletion of expressions until we have retrieved the expected type for
     this expression (via unification). This way, we can filter suggestions based on type. *)
  let decls = ctx.ctx_decls_table in
  let typ = Type.fresh_var ctx.ctx_current_level in
  request :=
    Some
      {
        ac_request_decls = decls;
        ac_request_prefix = prefix;
        ac_request_suffix = suffix;
        ac_request_type = Some typ;
        ac_request_loc = loc;
      };
  Tast_utils.mk_texpr Tast.Texpr_error typ Tast.Tclock_static Location.dummy

let autocomplete_type ctx loc prefix suffix =
  let types = ctx.ctx_types_table in

  let suggestions =
    StringMap.fold
      (fun name t acc ->
        let kind =
          match Type.prune t with
          | Tast.Ttype_enum _ -> ACitem_struct
          | _ -> ACitem_type
        in
        let suggestion =
          { ac_item_kind = kind; ac_item_label = name; ac_item_loc = loc }
        in
        suggestion :: acc)
      types []
  in

  raise (Autocomplete (filter_and_sort suggestions prefix suffix))

let can_unify t1 t2 =
  try
    Type.unify ~read_only:true t1 t2;
    true
  with Type.UnificationError _ -> false

let finish_autocompletation () =
  match !request with
  | None -> ()
  | Some req ->
      (* Filter suggestions based on type if provided *)
      let suggestions =
        StringMap.fold
          (fun name decl_kind acc ->
            let item =
              {
                ac_item_label = name;
                ac_item_loc = req.ac_request_loc;
                ac_item_kind =
                  (match decl_kind with
                  | Tast.Tdecl_function _ -> ACitem_function
                  | Tast.Tdecl_let _ | Tast.Tdecl_param _ -> ACitem_variable
                  | Tast.Tdecl_constructor _ -> ACitem_constructor
                  | Tast.Tdecl_global _ -> ACitem_variable);
              }
            in

            match req.ac_request_type with
            | None -> item :: acc
            | Some typ ->
                if can_unify (Tast_utils.type_of_decl decl_kind) typ then
                  item :: acc
                else acc)
          req.ac_request_decls []
      in

      raise
        (Autocomplete
           (filter_and_sort suggestions req.ac_request_prefix
              req.ac_request_suffix))
