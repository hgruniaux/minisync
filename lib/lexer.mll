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

{
  open Parser
  open Lexing

  exception Error of string

  let error msg = raise (Error msg)

  let buffer = Buffer.create 128

  let check_char_literal s =
    if String.length s > 1 then
      error "Character literal must be a single character."
    else if String.length s = 0 then
      error "Character literal cannot be empty."
    else
      s.[0]

  let resolve_keyword =
    let keywords = Hashtbl.create 23 in
    let add_keyword kw token = Hashtbl.add keywords kw token in
    let () =
      List.iter
        (fun (kw, token) -> add_keyword kw token)
        [
          ("and", AND);
          ("automaton", AUTOMATON);
          ("begin", BEGIN);
          ("continue", CONTINUE);
          ("do", DO);
          ("done", DONE);
          ("else", ELSE);
          ("end", END);
          ("every", EVERY);
          ("exception", EXCEPTION);
          ("fby", FBY);
          ("float", FLOAT);
          ("fun", FUN);
          ("if", IF);
          ("in", IN);
          ("include", INCLUDE);
          ("int", INT);
          ("last", LAST);
          ("let", LET);
          ("logand", LOGAND);
          ("logor", LOGOR);
          ("match", MATCH);
          ("merge", MERGE);
          ("mod", MOD);
          ("module", MODULE);
          ("node", NODE);
          ("not", NOT);
          ("open", OPEN);
          ("pre", PRE);
          ("rec", REC);
          ("then", THEN);
          ("try", TRY);
          ("type", TYPE);
          ("unless", UNLESS);
          ("until", UNTIL);
          ("val", VAL);
          ("when", WHEN);
          ("where", WHERE);
          ("with", WITH);
        ]
    in
    fun s -> try Hashtbl.find keywords s with Not_found -> LIDENTIFIER s


}

let whitespace = [' ' '\t']+
let eol = '\n' | '\r' '\n'

let lidentifier = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let uidentifier = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let bin_digit = ['0'-'1']
let oct_digit = ['0'-'7']
let dec_digit = ['0'-'9']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']

let bin_integer = '0' ('b' | 'B') bin_digit+
let oct_integer = '0' ('o' | 'O') oct_digit+
let hex_integer = '0' ('x' | 'X') hex_digit+
let dec_integer = ['0'-'9']+

let integer = bin_integer | oct_integer | hex_integer | dec_integer

let exponent = ('e' | 'E') ('+' | '-')? ['0'-'9']+
let float = (['0'-'9']+ '.' ['0'-'9']* exponent? | ['0'-'9']+ exponent)

rule next_token = parse
  | eof { EOF }
  | eol { new_line lexbuf; next_token lexbuf }
  | whitespace { next_token lexbuf }
  | "(*" { block_comment 1 lexbuf }
  | lidentifier as i { resolve_keyword i }
  | uidentifier as i { UIDENTIFIER i }
  | integer as i { (INTEGER_LITERAL (Z.of_string i)) }
  | float as f { (FLOAT_LITERAL (float_of_string f)) }
  | "\"" { Buffer.clear buffer; string_literal lexbuf; STRING_LITERAL (Buffer.contents buffer) }
  | "\'" { Buffer.clear buffer; char_literal lexbuf; CHAR_LITERAL (check_char_literal (Buffer.contents buffer)) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LSQUARE }
  | "]" { RSQUARE }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | ";" { SEMI }
  | ";;" { SEMI_SEMI }
  | "," { COMMA }
  | ":" { COLON }
  | "_" { WILDCARD }
  | "->" { ARROW }
  | "=>" { FAT_ARROW }
  | "|" { PIPE }
  | "=" { EQ }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "+." { PLUS_DOT }
  | "-." { MINUS_DOT }
  | "*." { STAR_DOT }
  | "/." { SLASH_DOT }
  | "==" { EQ_EQ }
  | "<>" { LESS_GREATER }
  | "<" { LESS_THAN }
  | ">" { GREATER_THAN }
  | "<=" { LESS_EQ }
  | ">=" { GREATER_EQ }
  | "&&" { AMP_AMP }
  | "||" { PIPE_PIPE }
  | "@" { AT }
  | _ as c { error (Format.asprintf "Unexpected character %c in source code." c) }

and block_comment depth = parse
  | eof { error "Unterminated block comment." }
  | eol { new_line lexbuf; block_comment depth lexbuf }
  | "(*" { block_comment (depth + 1) lexbuf }
  | "*)"
    {
      if depth = 1 then next_token lexbuf
      else block_comment (depth - 1) lexbuf
    }
  | _ { block_comment depth lexbuf }

and string_literal = parse
  | eol | eof { error "Unterminated string literal." }
  | "\\" { let c = escape_sequence lexbuf in Buffer.add_char buffer c; string_literal lexbuf }
  | "\"" { () }
  | _ as c { Buffer.add_char buffer c; string_literal lexbuf }

and char_literal = parse
  | eol | eof { error "Unterminated character literal." }
  | "\\" { let c = escape_sequence lexbuf in Buffer.add_char buffer c; char_literal lexbuf }
  | "\'" { () }
  | _ as c { Buffer.add_char buffer c; char_literal lexbuf }

and escape_sequence = parse
  | "n" { '\n' }
  | "t" { '\t' }
  | "b" { '\b' }
  | "r" { '\r' }
  | " " { ' ' }
  | "\\" { '\\' }
  | "\'" { '\'' }
  | "\"" { '"' }
  | (dec_digit dec_digit dec_digit as v)
    {
      let char_code = int_of_string v in
      Char.chr char_code
    }
  | "x" (hex_digit hex_digit as v)
    {
      let char_code = int_of_string ("0x" ^ v) in
      Char.chr char_code
    }
  | "o" (oct_digit oct_digit oct_digit as v)
    {
      let char_code = int_of_string ("0o" ^ v) in
      Char.chr char_code
    }
  | _ { error "Invalid escape sequence." }
