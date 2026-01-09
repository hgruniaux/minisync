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
open Tast
open Format
open Printer_utils

let print_decl_interface fmt d =
  let aux = function
    | Tdecl_let _ | Tdecl_param _ | Tdecl_constructor _ -> ()
    | Tdecl_global decl ->
        let name = decl.tdecl_global_name.value in
        fprintf fmt "@[%a %a : @[<hov>%a@]@]@," pp_keyword "val"
          (pp_string_with_color Variable)
          name Tast_printer.pp_type decl.tdecl_global_value.texpr_type;
        fprintf fmt "@[%a %a :: @[<hov>%a@]@]" pp_keyword "val"
          (pp_string_with_color Variable)
          name Tast_printer.pp_clock decl.tdecl_global_value.texpr_clock
    | Tdecl_function decl ->
        let name = decl.tdecl_fun_name.value in
        fprintf fmt "@[%a %a : @[<hov>%a@]@]@," pp_keyword "val"
          (pp_string_with_color Function)
          name Tast_printer.pp_type decl.tdecl_fun_type;
        fprintf fmt "@[%a %a :: @[<hov>%a@]@]" pp_keyword "val"
          (pp_string_with_color Function)
          name Tast_printer.pp_clock decl.tdecl_fun_clock
  in
  Tast_printer.enter_type_var_context ();
  aux d;
  Tast_printer.exit_type_var_context ()

let print_interface tast =
  let fmt = std_formatter in
  fprintf fmt "@[<v>";
  pp_print_list
    ~pp_sep:(fun fmt _ -> fprintf fmt "@,")
    print_decl_interface fmt tast;
  fprintf fmt "@]@."
