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

open Parser
open Lexing

let mk_pos p = (p.pos_lnum, p.pos_cnum - p.pos_bol)

type comparison = Before | Inside | After

let pos_compare (l1, c1) (l2, c2) =
  if l1 < l2 then -1
  else if l1 > l2 then 1
  else if c1 < c2 then -1
  else if c1 > c2 then 1
  else 0

let cursor_in_interval cursor start_pos end_pos =
  match (pos_compare cursor start_pos, pos_compare cursor end_pos) with
  | -1, _ -> Before
  | 0, _ -> Inside
  | 1, -1 -> Inside
  | 1, 0 -> Inside
  | 1, 1 -> After
  | _ -> assert false

let split_identifier_at_cursor s start_loc cursor_pos =
  let offset = snd cursor_pos - snd start_loc in
  let len = String.length s in
  let safe_off = max 0 (min len offset) in
  let prefix = String.sub s 0 safe_off in
  let suffix = String.sub s safe_off (len - safe_off) in
  (prefix, suffix)

let autocomplete_was_emitted = ref false
let pending_token = ref None

let rec next_token (line, col) lexbuf =
  let original_next_token = Lexer.next_token (* keep comments: *) true in

  let skip_comment token =
    match token with COMMENT -> next_token (line, col) lexbuf | _ -> token
  in

  match !pending_token with
  | Some tok ->
      pending_token := None;
      skip_comment tok
  | None -> (
      let token = original_next_token lexbuf in
      let token_start_pos = mk_pos lexbuf.lex_start_p in
      let token_end_pos = mk_pos lexbuf.lex_curr_p in
      let cursor_pos = (line, col - 1) in

      (* There is three cases:
          - The cursor is before the token (or at its start)
            => emit an autocomplete token
          - The cursor is after the token (or at its end)
            => just return the token, the autocomplete will be handled later.
          - The cursor is inside the token, some work must be done.
            => If the token is a literal or a comment, ignore the autocompletation.
            => If the token is an identifier, split it and emit a special
                autocomplete identifier token.
            => Otherwise, just emit the token and ignore the autocompletation.
          In all cases, the COMMENT token must not be returned. *)
      match cursor_in_interval cursor_pos token_start_pos token_end_pos with
      | Before when not !autocomplete_was_emitted ->
          autocomplete_was_emitted := true;
          pending_token := Some token;
          AUTOCOMPLETE
      | Inside when not !autocomplete_was_emitted -> (
          autocomplete_was_emitted := true;
          match token with
          | LIDENTIFIER s ->
              let prefix, suffix =
                split_identifier_at_cursor s token_start_pos cursor_pos
              in
              AUTOCOMPLETE_LIDENTIFIER (prefix, suffix)
          | UIDENTIFIER s ->
              let prefix, suffix =
                split_identifier_at_cursor s token_start_pos cursor_pos
              in
              AUTOCOMPLETE_UIDENTIFIER (prefix, suffix)
          | INTEGER_LITERAL _ | FLOAT_LITERAL _ | STRING_LITERAL _
          | CHAR_LITERAL _ ->
              token
          | COMMENT when cursor_pos = token_end_pos -> AUTOCOMPLETE
          | _ when cursor_pos = token_end_pos ->
              pending_token := Some AUTOCOMPLETE;
              token
          | _ when cursor_pos = token_start_pos ->
              pending_token := Some token;
              AUTOCOMPLETE
          | _ -> skip_comment token)
      | _ -> skip_comment token)
