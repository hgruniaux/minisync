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
module StringMap = Map.Make (String)

exception Error of string * Location.t * (string * Location.t) list

(** True if some errors have occurred during typechecking. This is required as
    some errors exceptions may be catched to build a partial typed AST. So, this
    flag is used to track that. *)
let had_error = ref false

let error msg pos = raise (Error (msg, pos, []))

let error_with_note ?(ignore_note_if_dummy = false) msg pos note_msg note_pos =
  let notes =
    if ignore_note_if_dummy && Location.is_dummy note_pos then []
    else [ (note_msg, note_pos) ]
  in
  raise (Error (msg, pos, notes))

let error_with_two_notes ?(ignore_note_if_dummy = false) msg pos first_note
    first_pos second_note second_pos =
  let notes =
    if
      ignore_note_if_dummy
      && (Location.is_dummy first_pos || Location.is_dummy second_pos)
    then []
    else [ (first_note, first_pos); (second_note, second_pos) ]
  in
  raise (Error (msg, pos, notes))

let warning msg pos =
  let is_error = Error.print_warning msg pos [] in
  if is_error then had_error := true

let deprecated_usage decl_type_name decl_name deprecated_kind =
  match deprecated_kind with
  | Deprecated ->
      let msg =
        Format.asprintf "%s '%s' is deprecated." decl_type_name
          decl_name.Ast.value
      in
      warning msg decl_name.loc
  | Deprecated_with_message message ->
      let msg =
        Format.asprintf "%s '%s' is deprecated: %s" decl_type_name
          decl_name.Ast.value message
      in
      warning msg decl_name.loc
  | Not_deprecated -> ()

type context = {
  mutable ctx_decls_table : declaration StringMap.t;
  mutable ctx_types_table : ttype StringMap.t;
  mutable ctx_current_level : int;
      (** Current level of type and clock variables. *)
}

(** Creates a new typechecking context with builtin declarations and types. *)
let create_context () : context =
  let false_ctor = Type.false_constructor in
  let true_ctor = Type.true_constructor in
  {
    ctx_decls_table =
      StringMap.(
        empty
        |> add false_ctor.tdecl_constructor_name.value
             (Tdecl_constructor false_ctor)
        |> add true_ctor.tdecl_constructor_name.value
             (Tdecl_constructor true_ctor));
    ctx_types_table =
      StringMap.(
        empty
        |> add Type.bool_declaration.tdecl_enum_name.value
             (Ttype_enum Type.bool_declaration)
        |> add Type.unit_declaration.tdecl_enum_name.value
             (Ttype_enum Type.unit_declaration));
    ctx_current_level = 0;
  }

(** Lookups the given symbol [spelling] in the given [map]. If the symbol is not
    found, [error_msg] is called with [spelling] to generate an error message
    and its location that will be used for [error_with_note] to emit an error
    with possible suggestions of different available spellings in [map]. *)
let lookup ~error_msg ?(bonus = fun _ -> 0) map (spelling : Ast.identifier) =
  match StringMap.find_opt spelling.value map with
  | Some d -> d
  | None ->
      (* The symbol was not found in the map. Try to detect spelling errors and suggest
         near symbols in the map. *)

      (* Compute the max edit distance allowed for spellchecking suggestions.
         We want to allow small edit distance for small symbols, but larger
         ones for larger symbols. *)
      let max_dist symbol =
        let n = String.length symbol in
        if n <= 2 then 1 else if n <= 5 then 2 else 3
      in

      (* Compute spellchecking suggestions. *)
      let limit = max_dist spelling.value + 1 in
      let suggestions =
        StringMap.fold
          (fun symbol decl suggestions ->
            let distance = String.edit_distance ~limit spelling.value symbol in
            if distance < limit then
              let b = bonus decl in
              (symbol, distance - b) :: suggestions
            else suggestions)
          map []
      in

      if List.is_empty suggestions then
        let msg, loc = error_msg spelling in
        error msg loc
      else
        (* Sort suggestions by distance (best suggestions first). *)
        let sorted_suggestions =
          List.sort (fun (_, d1) (_, d2) -> compare d1 d2) suggestions
        in

        (* Only keep the first suggestions, in case there are many. *)
        let max_suggestions = 4 in
        let best_suggestions = List.take max_suggestions sorted_suggestions in

        (* Then compute the note message. *)
        let rec pp_english_list pp fmt = function
          | [] -> ()
          | [ a ] -> pp fmt a
          | [ a; b ] -> Format.fprintf fmt "%a or %a" pp a pp b
          | a :: tl -> Format.fprintf fmt "%a, %a" pp a (pp_english_list pp) tl
        in

        let note_msg =
          Format.asprintf "Did you mean %a?"
            (pp_english_list (fun fmt (s, _) -> Format.fprintf fmt "%s" s))
            best_suggestions
        in
        let note_loc = Location.dummy in
        let msg, loc = error_msg spelling in
        error_with_note msg loc note_msg note_loc

let%test_unit "lookup test" =
  let map =
    StringMap.(
      empty |> add "foo" 0 |> add "foo1" 1 |> add "bar" 2 |> add "baz" 3
      |> add "qux" 4)
  in
  let symbol name = { Ast.value = name; loc = Location.dummy } in
  let id_foo = symbol "foo" in
  let id_baz = symbol "baz" in
  let id_fop = symbol "fop" in
  assert (lookup ~error_msg:(fun id -> ("Not found", id.loc)) map id_foo = 0);
  assert (lookup ~error_msg:(fun id -> ("Not found", id.loc)) map id_baz = 3);
  try
    let _ = lookup ~error_msg:(fun id -> ("Not found", id.loc)) map id_fop in
    assert false
  with Error (msg, _, notes) ->
    assert (msg = "Not found");
    assert (List.length notes = 1);
    let note_msg, _ = List.hd notes in
    assert (note_msg = "Did you mean foo or foo1?")
