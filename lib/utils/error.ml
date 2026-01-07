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

open Format
open Printer_utils

type format = OCaml | GCC

let format = ref OCaml
let show_lines = ref true
let show_warnings = ref true
let warnings_as_errors = ref false

(** [get_line_in_file filename line_number] returns the content of the specified
    line number [line_number] in the file [filename], or [None] if the line does
    not exist or if there was an error reading the file. *)
let get_line_in_file filename line_number =
  try
    let ic = open_in filename in
    let rec aux n =
      if n = line_number then input_line ic
      else (
        ignore (input_line ic);
        aux (n + 1))
    in
    let line = aux 1 in
    close_in ic;
    Some line
  with
  | End_of_file -> None
  | Sys_error _ -> None

(** [parse_position pos] converts a Lexing.position [pos] to a tuple of line and
    column numbers (both 1-based). *)
let parse_position pos =
  let line = pos.Lexing.pos_lnum in
  let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
  (line, column)

(** [print_header_ocaml fmt pos] prints the message location locus in the OCaml
    compiler style. *)
let print_header_ocaml fmt pos =
  if Location.is_dummy pos then ()
  else
    let start_pos, end_pos = pos in
    let file = start_pos.Lexing.pos_fname in
    let start_line, start_col = parse_position start_pos in
    let end_line, end_col = parse_position end_pos in
    pp_open_stag fmt DiagLocus;
    fprintf fmt "File \"%s\", " file;
    if start_line = end_line then (
      fprintf fmt "line %d" start_line;
      if start_col = end_col then fprintf fmt ", character %d" start_col
      else fprintf fmt ", characters %d-%d" start_col end_col)
    else fprintf fmt "lines %d-%d" start_line end_line;
    fprintf fmt ":";
    pp_close_stag fmt ();
    fprintf fmt "\n"

(** [print_header_ocaml fmt pos] prints the message location locus in the GCC
    compiler style. *)
let print_header_gcc fmt pos =
  if Location.is_dummy pos then ()
  else
    let start_pos, end_pos = pos in
    let file = start_pos.Lexing.pos_fname in
    let start_line, start_col = parse_position start_pos in
    let end_line, end_col = parse_position end_pos in

    pp_open_stag fmt DiagLocus;
    fprintf fmt "%s:" file;
    if start_line = end_line then (
      fprintf fmt "%d:" start_line;
      if start_col = end_col then fprintf fmt "%d:" start_col
      else fprintf fmt "%d-%d:" start_col end_col)
    else fprintf fmt "%d-%d:" start_line end_line;
    pp_close_stag fmt ();
    fprintf fmt " "

(** [print_line ~eol_before fmt pos color] prints the source code line
    corresponding to [pos] (if it spans a single source line) in the provided
    [color]. If [eol_before] is true, then a EOL is emitted before the source
    line, otherwise after. *)
let print_line ~eol_before fmt pos color =
  if Location.is_dummy pos then ()
  else
    let start_pos, end_pos = pos in
    let file = start_pos.Lexing.pos_fname in
    let start_line, start_col = parse_position start_pos in
    let end_line, end_col = parse_position end_pos in
    if start_line = end_line then
      match get_line_in_file file start_line with
      | Some line ->
          if eol_before then fprintf fmt "\n";
          let margin = asprintf " %d | " start_line in
          let span = if end_col = start_col then 1 else end_col - start_col in
          fprintf fmt "%s%s\n" margin line;
          fprintf fmt "%s" (String.make (String.length margin) ' ');
          fprintf fmt "%s" (String.make (start_col - 1) ' ');
          pp_open_stag fmt color;
          fprintf fmt "%s" (String.make span '^');
          pp_close_stag fmt ();
          if not eol_before then fprintf fmt "\n"
      | None -> ()

let print_message fmt severity color msg pos =
  match !format with
  | OCaml ->
      print_header_ocaml fmt pos;
      if !show_lines then print_line ~eol_before:false fmt pos color;
      fprintf fmt "@[";
      pp_string_with_color color fmt severity;
      fprintf fmt ": @[<hov>%a@]@]" pp_print_text msg
  | GCC ->
      fprintf fmt "@[";
      print_header_gcc fmt pos;
      pp_string_with_color color fmt severity;
      fprintf fmt ": @[<hov>%a@]@]" pp_print_text msg;
      if !show_lines then print_line ~eol_before:true fmt pos color

let print_error_at fmt msg pos = print_message fmt "Error" DiagError msg pos

let print_warning_at fmt msg pos =
  print_message fmt "Warning" DiagWarning msg pos

let print_note_at fmt msg pos = print_message fmt "Note" DiagNote msg pos

let print_notes fmt notes =
  List.iter
    (fun (note_msg, note_pos) ->
      Format.fprintf fmt "\n";
      print_note_at fmt note_msg note_pos)
    notes

let print_error msg loc notes =
  let fmt = Format.err_formatter in
  print_error_at fmt msg loc;
  print_notes fmt notes;
  pp_print_flush fmt ();
  Format.fprintf fmt "@."

let print_warning msg loc notes =
  if !show_warnings then (
    let fmt = Format.err_formatter in
    if !warnings_as_errors then print_error_at fmt msg loc
    else print_warning_at fmt msg loc;
    print_notes fmt notes;
    Format.fprintf fmt "@.";
    !warnings_as_errors)
  else false
