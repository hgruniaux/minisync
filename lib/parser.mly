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

%{
  open Ast

  let mk_ident s loc = { value = s; loc }
  let mk_type kind loc = { value = kind; loc }
  let mk_binop kind loc = { value = kind; loc }
  let mk_unop kind loc = { value = kind; loc }
  let mk_expr kind loc = { value = kind; loc }
  let mk_decl kind loc = { value = kind; loc }
%}

(* Special tokens *)
%token EOF
%token<string> LIDENTIFIER
%token<string> UIDENTIFIER
%token<Z.t> INTEGER_LITERAL
%token<float> FLOAT_LITERAL
%token<string> STRING_LITERAL
%token<char> CHAR_LITERAL

(* Keywords *)
%token AND "and"
%token AUTOMATON "automaton"
%token BEGIN "begin"
%token CONTINUE "continue"
%token DO "do"
%token DONE "done"
%token ELSE "else"
%token END "end"
%token EVERY "every"
%token EXCEPTION "exception"
%token FBY "fby"
%token FLOAT "float"
%token FUN "fun"
%token IF "if"
%token IN "in"
%token INCLUDE "include"
%token INT "int"
%token LAST "last"
%token LET "let"
%token LOGAND "logand"
%token LOGOR "logor"
%token MATCH "match"
%token MERGE "merge"
%token MOD "mod"
%token MODULE "module"
%token NODE "node"
%token NOT "not"
%token OPEN "open"
%token PRE "pre"
%token REC "rec"
%token THEN "then"
%token TRY "try"
%token TYPE "type"
%token UNLESS "unless"
%token UNTIL "until"
%token VAL "val"
%token WHEN "when"
%token WHERE "where"
%token WITH "with"

(* Operators and punctuations *)
%token LPAREN "("
%token RPAREN ")"
%token LSQUARE "["
%token RSQUARE "]"
%token LBRACE "{"
%token RBRACE "}"
%token SEMI ";"
%token SEMI_SEMI ";;"
%token COMMA ","
%token COLON ":"
%token WILDCARD "_"
%token ARROW "->"
%token FAT_ARROW "=>"
%token PIPE "|"
%token EQ "="
%token PLUS "+"
%token MINUS "-"
%token STAR "*"
%token SLASH "/"
%token PLUS_DOT "+."
%token MINUS_DOT "-."
%token STAR_DOT "*."
%token SLASH_DOT "/."
%token EQ_EQ "=="
%token LESS_GREATER "<>"
%token LESS_THAN "<"
%token GREATER_THAN ">"
%token LESS_EQ "<="
%token GREATER_EQ ">="
%token AMP_AMP "&&"
%token PIPE_PIPE "||"
%token AT "@"

%nonassoc LOW
%nonassoc below_AND
%left AND
%nonassoc LPAREN
%nonassoc below_SEMI
%right SEMI

%start<Ast.declaration list> program

%%

program:
  | decls=declaration* EOF { decls }

(* --------------------------------------------------------
 * Types
 * -------------------------------------------------------- *)

atomic_type:
  | "int" { mk_type Atype_int $sloc }
  | "float" { mk_type Atype_float $sloc }
  | id=lidentifier { mk_type (Atype_name id) $sloc }
  | "(" t=type_name ")" { mk_type (Atype_paren t) $sloc }

product_type:
  | t=atomic_type { t }
  | l=atomic_type "*" r=product_type
    {
      match r.value with
      | Atype_tuple types ->
          mk_type (Atype_tuple (l :: types)) $sloc
      | _ ->
          mk_type (Atype_tuple [l; r]) $sloc
    }

function_type:
  | t=product_type { t }
  | param=product_type "->" ret=function_type
    {
      match ret.value with
      | Atype_function (params, r) ->
          mk_type (Atype_function (param :: params, r)) $sloc
      | _ ->
          mk_type (Atype_function ([param], ret)) $sloc
    }

node_type:
  | t=function_type { t }
  | params=function_type "=>" ret=function_type
    {
      match params.value with
      | Atype_function (params_list, r) ->
          mk_type (Atype_node (params_list @ [r], ret)) $sloc
      | _ ->
          mk_type (Atype_node ([params], ret)) $sloc
    }

type_name:
  | t=node_type { t }

(* --------------------------------------------------------
 * Attributes
 * -------------------------------------------------------- *)

attribute:
  | "@" name=identifier "(" args=separated_list(",", primary_expression) ")"
    { { attr_name = name; attr_args = args } }
  | "@" name=identifier
    { { attr_name = name; attr_args = [] } }

attribute_declaration:
  | "[" attrs=separated_list(";", attribute) "]"
    { attrs }
  | /* empty */ { [] }

(* --------------------------------------------------------
 * Declarations
 * -------------------------------------------------------- *)

declaration:
  | attrs=attribute_declaration d=declaration_kind { d, attrs }

declaration_kind:
  | d=value_declaration { d }
  | d=global_declaration { d }
  | d=function_declaration { d }
  | d=node_declaration { d }
  | d=type_declaration { d }

value_declaration:
  | "val" id=lidentifier ":" t=type_name { mk_decl (Adecl_value (id, t)) $sloc }

global_declaration:
  | "let" id=lidentifier "=" value=seq_expression { mk_decl (Adecl_global (id, value)) $sloc }

function_declaration:
  | "let" id=lidentifier params=pattern* "=" body=seq_expression
    { mk_decl (Adecl_function (id, params, body, false)) $sloc }

node_declaration:
  | "let" "node" id=lidentifier params=pattern* "=" body=seq_expression
    { mk_decl (Adecl_function (id, params, body, true)) $sloc }

(* --------------------------------------------------------
 * Type declarations
 * -------------------------------------------------------- *)

type_declaration:
  | "type" items=separated_list("and", type_declaration_item)
    { mk_decl (Adecl_type items) $sloc }

type_declaration_item:
  | id=lidentifier "=" d=type_definition
    { (id, d) }

type_definition:
  | d=type_alias_definition { d }
  | d=type_enum_definition { d }

type_alias_definition:
  | target=type_name
    { Atype_decl_alias target }

type_enum_definition:
  | "|"? variants=separated_nonempty_list("|", uidentifier)
    { Atype_decl_enum variants }

(* --------------------------------------------------------
 * Expressions
 * -------------------------------------------------------- *)

primary_expression:
  | i=INTEGER_LITERAL { mk_expr (Aexpr_int i) $sloc }
  | f=FLOAT_LITERAL { mk_expr (Aexpr_float f) $sloc }
  | s=STRING_LITERAL { mk_expr (Aexpr_string s) $sloc }
  | c=CHAR_LITERAL { mk_expr (Aexpr_char c) $sloc }
  | i=lidentifier { mk_expr (Aexpr_var i) $sloc }
  | i=uidentifier %prec LOW { mk_expr (Aexpr_var i) $sloc }
  | "(" ")" { mk_expr (Aexpr_unit) $sloc }
  | "begin" e=seq_expression "end" { mk_expr (Aexpr_paren e) $sloc }
  | "(" e=seq_expression ")" { mk_expr (Aexpr_paren e) $sloc }
  | "(" e=seq_expression "," es=separated_list(",", seq_expression) ")"
    { mk_expr (Aexpr_tuple (e :: es)) $sloc }
  | "automaton" "|"? _branches=automaton_branches "end" { failwith "TODO: automaton" }
  | "match" expr=seq_expression "with" "|"? branches=match_branches "end"
    { mk_expr (Aexpr_match (expr, branches)) $sloc }

match_branches:
  | branches=separated_nonempty_list("|", match_branch) { branches }

match_branch:
  | p=pattern "->" expr=seq_expression { (p, expr) }

automaton_branches:
  | branches=separated_nonempty_list("|", automaton_branch) { branches }

automaton_branch:
  | identifier "->" "do" seq_expression "done" {()}
  | identifier "->" "do" seq_expression preemption {()}

preemption:
  | "then" identifier { [] }
  | "continue" identifier { [] }
  | l=preemtion_item+ { l }

preemtion_item:
  | "unless" primary_expression "then" identifier     { (false, false) }
  | "unless" primary_expression "continue" identifier { (false, true)  }
  | "until" primary_expression "then" identifier      { (true,  false) }
  | "until" primary_expression "continue" identifier  { (true,  false) }

postfix_expression:
  | expr=primary_expression { expr }
  | callee=primary_expression args=primary_expression+
    { mk_expr (Aexpr_call (callee, args)) $sloc }

unary_expression:
  | expr=postfix_expression { expr }
  | op=unary_operator expr=unary_expression
    { mk_expr (Aexpr_unary (op, expr)) $sloc }

%inline unary_operator:
  | "-" { mk_unop Aunop_neg $sloc }
  | "-." { mk_unop Aunop_fneg $sloc }
  | "not" { mk_unop Aunop_not $sloc }
  | "pre" { mk_unop Aunop_pre $sloc }
  | "last" { mk_unop Aunop_last $sloc } (* FIXME: last should only accept identifiers *)

every_expression:
  | expr=unary_expression { expr }
  | call=unary_expression _kw="every" clock=every_expression
    { mk_expr (Aexpr_every (call, clock, $loc(_kw))) $sloc }

fby_expression:
  | expr=every_expression { expr }
  | initial=every_expression _kw="fby" step=fby_expression
    { mk_expr (Aexpr_binary (mk_binop Abinop_fby $loc(_kw), initial, step)) $sloc }

when_expression:
  | expr=fby_expression { expr }
  | expr=fby_expression _kw="when" cond=when_expression
    { mk_expr (Aexpr_when_bool (expr, cond, $loc(_kw))) $sloc }
  | expr=fby_expression _kw="when" id=uidentifier "(" cond=expression ")"
    { mk_expr (Aexpr_when (expr, id, cond, $loc(_kw))) $sloc }

merge_expression:
  | expr=when_expression { expr }
  | _kw="merge" cond=primary_expression branches=merge_branch+
    { mk_expr (Aexpr_merge (cond, branches, $loc(_kw))) $sloc }

initialization_expression:
  | expr=merge_expression { expr }
  | initial=merge_expression op=initialization_operator step=initialization_expression
    { mk_expr (Aexpr_binary (op, initial, step)) $sloc }

%inline initialization_operator:
  | "->" { mk_binop Abinop_init $sloc }

multiplicative_expression:
  | expr=initialization_expression { expr }
  | lhs=multiplicative_expression op=multiplicative_operator rhs=initialization_expression
    { mk_expr (Aexpr_binary (op, lhs, rhs)) $sloc }

%inline multiplicative_operator:
  | "*" { mk_binop Abinop_mul $sloc }
  | "/" { mk_binop Abinop_div $sloc }
  | "mod" { mk_binop Abinop_mod $sloc }
  | "*." { mk_binop Abinop_fmul $sloc }
  | "/." { mk_binop Abinop_fdiv $sloc }

additive_expression:
  | expr=multiplicative_expression { expr }
  | lhs=additive_expression op=additive_operator rhs=multiplicative_expression
    { mk_expr (Aexpr_binary (op, lhs, rhs)) $sloc }

%inline additive_operator:
  | "+" { mk_binop Abinop_add $sloc }
  | "-" { mk_binop Abinop_sub $sloc }
  | "+." { mk_binop Abinop_fadd $sloc }
  | "-." { mk_binop Abinop_fsub $sloc }

relational_expression:
  | expr=additive_expression { expr }
  | lhs=relational_expression op=relational_operator rhs=additive_expression
    { mk_expr (Aexpr_binary (op, lhs, rhs)) $sloc }

%inline relational_operator:
  | "<"  { mk_binop Abinop_lt $sloc }
  | ">"  { mk_binop Abinop_gt $sloc }
  | "<=" { mk_binop Abinop_le $sloc }
  | ">=" { mk_binop Abinop_ge $sloc }

equality_expression:
  | expr=relational_expression { expr }
  | lhs=equality_expression op=equality_operator rhs=relational_expression
    { mk_expr (Aexpr_binary (op, lhs, rhs)) $sloc }

%inline equality_operator:
  | "==" { mk_binop Abinop_eq $sloc }
  | "<>" { mk_binop Abinop_ne $sloc }

and_expression:
  | expr=equality_expression { expr }
  | lhs=and_expression op=and_operator rhs=equality_expression
    { mk_expr (Aexpr_binary (op, lhs, rhs)) $sloc }

%inline and_operator:
  | "&&" { mk_binop Abinop_logand $sloc }
  | "logand" { mk_binop Abinop_logand $sloc }

or_expression:
  | expr=and_expression { expr }
  | lhs=or_expression op=or_operator rhs=and_expression
    { mk_expr (Aexpr_binary (op, lhs, rhs)) $sloc }

%inline or_operator:
  | "||" { mk_binop Abinop_logor $sloc }
  | "logor" { mk_binop Abinop_logor $sloc }

expression:
  | expr=or_expression { expr }
  | "if" cond=seq_expression "then" then_branch=seq_expression "else" else_branch=expression
    { mk_expr (Aexpr_ite (cond, then_branch, else_branch)) $sloc }
  | expr=let_expression { expr }
  | expr=where_expression { expr }

seq_expression:
  | expr=expression %prec below_SEMI { expr }
  | first=expression ";" second=seq_expression
    { mk_expr (Aexpr_seq (first, second)) $sloc }

merge_branch:
  | "(" p=pattern "->" expr=expression ")"
    { (p, expr) }

let_binding:
  | p=pattern "=" value=seq_expression
    { (p, value) }

let_bindings:
  | b=let_binding %prec below_AND { [b] }
  | b=let_binding AND bs=let_bindings { b :: bs }

let_expression:
  | "let" bindings=let_bindings "in" body=seq_expression
    { mk_expr (Aexpr_let (bindings, body, None)) $sloc }
  | "let" _kw="rec" bindings=let_bindings "in" body=seq_expression
    { mk_expr (Aexpr_let (bindings, body, Some $loc(_kw))) $sloc }

where_expression:
  | body=primary_expression "where" bindings=let_bindings
    { mk_expr (Aexpr_where (bindings, body, None)) $sloc }
  | body=primary_expression "where" _kw="rec" bindings=let_bindings
    { mk_expr (Aexpr_where (bindings, body, Some $loc(_kw))) $sloc }

pattern:
  | "(" ")" { mk_expr (Apat_unit) $sloc }
  | "_" { mk_expr (Apat_wildcard) $sloc }
  | id=identifier { mk_expr (Apat_ident id) $sloc }
  | "(" p=pattern ")" { p }
  | "(" p=pattern ":" t=type_name ")" { mk_expr (Apat_typed (p, t)) $sloc }
  | "(" p=pattern "," ps=separated_list(",", pattern) ")"
    { mk_expr (Apat_tuple (p :: ps)) $sloc }

%inline identifier:
  | LIDENTIFIER { mk_ident $1 $sloc }
  | UIDENTIFIER { mk_ident $1 $sloc }

%inline lidentifier:
  | LIDENTIFIER { mk_ident $1 $sloc }

%inline uidentifier:
  | UIDENTIFIER { mk_ident $1 $sloc }
