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

let print_types = ref false
let print_clocks = ref false
let print_var_types_ids = ref false
let print_locations = ref false

type stag +=
  | Error
  | Comment
  | Keyword
  | Constant
  | Function
  | Type
  | TypeVariable
  | Constructor
  | Variable
  | Location
  | LabelExpr
  | LabelDecl
  | DiagError
  | DiagWarning
  | DiagNote
  | DiagLocus

let enable_colors fmt =
  pp_set_tags fmt true;

  let stag_functions =
    {
      print_open_stag = (fun _stag -> ());
      print_close_stag = (fun _stag -> ());
      mark_open_stag =
        (function
        | Error -> "\x1b[41m"
        | Comment -> "\x1b[90m"
        | Keyword -> "\x1b[31m"
        | Constant -> "\x1b[32m"
        | Function -> "\x1b[35m"
        | Type -> "\x1b[35m"
        | TypeVariable -> "\x1b[36m"
        | Constructor -> "\x1b[34m"
        | Variable -> "\x1b[36m"
        | Location -> "\x1b[33m"
        | LabelExpr -> "\x1b[34m"
        | LabelDecl -> "\x1b[32m"
        | DiagError -> "\x1b[1;31m"
        | DiagWarning -> "\x1b[1;33m"
        | DiagNote -> "\x1b[1;34m"
        | DiagLocus -> "\x1b[1m"
        | _ -> "");
      mark_close_stag =
        (function
        | Error | Comment | Keyword | Constant | Constructor | Variable
        | Function | Type | Location | TypeVariable | LabelExpr | LabelDecl
        | DiagError | DiagWarning | DiagNote | DiagLocus ->
            "\x1b[0m"
        | _ -> "");
    }
  in

  pp_set_formatter_stag_functions fmt stag_functions

let pp_with_color tag pp fmt fmt_arg =
  pp_open_stag fmt tag;
  pp fmt fmt_arg;
  pp_close_stag fmt ()

let pp_int fmt n = pp_with_color Constant Z.pp_print fmt n
let pp_float fmt f = pp_with_color Constant pp_print_float fmt f

let pp_string fmt s =
  pp_with_color Constant (fun fmt s -> fprintf fmt "%S" s) fmt s

let pp_char fmt c =
  pp_with_color Constant (fun fmt c -> fprintf fmt "%C" c) fmt c

let pp_string_with_color tag fmt str = pp_with_color tag pp_print_string fmt str
let pp_comment fmt c = pp_string_with_color Comment fmt c
let pp_keyword fmt str = pp_string_with_color Keyword fmt str
