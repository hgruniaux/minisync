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

open Minisync
open Format
open Autocompleter

let print_item_kind fmt = function
  | ACitem_type -> fprintf fmt "type"
  | ACitem_function -> fprintf fmt "function"
  | ACitem_variable -> fprintf fmt "variable"
  | ACitem_constructor -> fprintf fmt "constructor"
  | ACitem_enum -> fprintf fmt "enum"
  | ACitem_struct -> fprintf fmt "struct"
  | ACitem_field -> fprintf fmt "field"

let print_item_location fmt loc =
  let open Lexing in
  let start_pos, end_pos = loc in
  let start_line, end_line = (start_pos.pos_lnum, end_pos.pos_lnum) in
  let start_col = start_pos.pos_cnum - start_pos.pos_bol in
  let end_col = end_pos.pos_cnum - end_pos.pos_bol in
  if start_line = end_line then
    fprintf fmt "<%d:%d-%d>" start_line start_col end_col
  else fprintf fmt "<%d:%d-%d:%d>" start_line start_col end_line end_col

let print_suggestions suggestions =
  let fmt = Format.std_formatter in
  fprintf fmt "Autocomplete suggestions:\n";
  List.iter (fun item -> fprintf fmt " - %s\n" item.ac_item_label) suggestions
