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

type otype =
  | Otype_var of int
  | Otype_unit
  | Otype_bool
  | Otype_int
  | Otype_float
  | Otype_char
  | Otype_string
  | Otype_struct of ostruct
  | Otype_sum of osum
  | Otype_tuple of otype list
  | Otype_function of otype list * otype

and ostruct = {
  ostruct_name : string;
  ostruct_fields : (string * bool * otype) list;
}

and osum = { osum_name : string; osum_variants : string list }

and oexpression =
  | Oexpr_unit
  | Oexpr_bool of bool
  | Oexpr_int of Z.t
  | Oexpr_float of float
  | Oexpr_char of Char.t
  | Oexpr_string of string
  | Oexpr_var of string
  | Oexpr_tuple of oexpression list
  | Oexpr_func of ofunction
  | Oexpr_call of oexpression * oexpression list
  | Oexpr_struct_create of ostruct * (string * oexpression) list
  | Oexpr_struct_field of oexpression * string
  | Oexpr_struct_assign of oexpression * oexpression
  | Oexpr_let of bool * (opattern * oexpression) list * oexpression
  | Oexpr_seq of oexpression list
  | Oexpr_ite of oexpression * oexpression * oexpression
  | Oexpr_match of oexpression * (string * oexpression) list
  | Oexpr_binary of string * oexpression * oexpression
  | Oexpr_unary of string * oexpression

and opattern = Opat_unit | Opat_var of string | Opat_tuple of opattern list

and ofunction = {
  ofunction_name : string;
  ofunction_params : (string * otype) list;
  mutable ofunction_body : oexpression;
}

and omodule = {
  mutable omodule_types_to_be_defined : otype list;
  omodule_sum_types : (string, osum) Hashtbl.t;
  omodule_struct_types : (string, ostruct) Hashtbl.t;
  mutable omodule_functions : ofunction list;
}
