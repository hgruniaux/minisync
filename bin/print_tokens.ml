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
open Minisync
open Parser

let print_token fmt = function
  | EOF -> fprintf fmt "EOF"
  | LIDENTIFIER s -> fprintf fmt "LIDENTIFIER (%S)" s
  | UIDENTIFIER s -> fprintf fmt "UIDENTIFIER (%S)" s
  | INTEGER_LITERAL d -> fprintf fmt "INTEGER_LITERAL (%a)" Z.pp_print d
  | FLOAT_LITERAL f -> fprintf fmt "FLOAT_LITERAL (%f)" f
  | STRING_LITERAL s -> fprintf fmt "STRING_LITERAL (%S)" s
  | CHAR_LITERAL c -> fprintf fmt "CHAR_LITERAL (%C)" c
  | AUTOCOMPLETE -> fprintf fmt "AUTOCOMPLETE"
  | AUTOCOMPLETE_LIDENTIFIER (prefix, suffix) ->
      fprintf fmt "AUTOCOMPLETE_LIDENT (%S, %S)" prefix suffix
  | AUTOCOMPLETE_UIDENTIFIER (prefix, suffix) ->
      fprintf fmt "AUTOCOMPLETE_UIDENTIFIER (%S, %S)" prefix suffix
  | COMMENT -> fprintf fmt "COMMENT"
  | LPAREN -> fprintf fmt "LPAREN"
  | RPAREN -> fprintf fmt "RPAREN"
  | LBRACE -> fprintf fmt "LBRACE"
  | RBRACE -> fprintf fmt "RBRACE"
  | LSQUARE -> fprintf fmt "LSQUARE"
  | RSQUARE -> fprintf fmt "RSQUARE"
  | SEMI -> fprintf fmt "SEMI"
  | SEMI_SEMI -> fprintf fmt "SEMI_SEMI"
  | COMMA -> fprintf fmt "COMMA"
  | COLON -> fprintf fmt "COLON"
  | WILDCARD -> fprintf fmt "WILDCARD"
  | ARROW -> fprintf fmt "ARROW"
  | FAT_ARROW -> fprintf fmt "FAT_ARROW"
  | PIPE -> fprintf fmt "PIPE"
  | EQ -> fprintf fmt "EQ"
  | PLUS -> fprintf fmt "PLUS"
  | MINUS -> fprintf fmt "MINUS"
  | STAR -> fprintf fmt "STAR"
  | SLASH -> fprintf fmt "SLASH"
  | PLUS_DOT -> fprintf fmt "PLUS_DOT"
  | MINUS_DOT -> fprintf fmt "MINUS_DOT"
  | STAR_DOT -> fprintf fmt "STAR_DOT"
  | SLASH_DOT -> fprintf fmt "SLASH_DOT"
  | EQ_EQ -> fprintf fmt "EQ_EQ"
  | LESS_GREATER -> fprintf fmt "LESS_GREATER"
  | LESS_EQ -> fprintf fmt "LESS_EQ"
  | GREATER_EQ -> fprintf fmt "GREATER_EQ"
  | LESS_THAN -> fprintf fmt "LESS_THAN"
  | GREATER_THAN -> fprintf fmt "GREATER_THAN"
  | AMP_AMP -> fprintf fmt "AMP_AMP"
  | PIPE_PIPE -> fprintf fmt "PIPE_PIPE"
  | AT -> fprintf fmt "AT"
  | _ -> fprintf fmt "UNKNOWN_TOKEN"

let print_tokens next_token lexbuf =
  let rec aux () =
    let token = next_token lexbuf in
    print_token Format.std_formatter token;
    printf "@,";
    if token <> EOF then aux ()
  in
  printf "@[<v>";
  aux ();
  printf "@]"
