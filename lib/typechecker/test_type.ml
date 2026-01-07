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
open Type

let mk_ident n = { Ast.value = n; loc = Location.dummy }

let%test_unit "type pruning" =
  assert (prune bool_type == bool_type);
  assert (prune Ttype_bottom == Ttype_bottom);
  assert (prune (Ttype_paren Ttype_int) == Ttype_int);

  let named_type = Ttype_named (mk_ident "MyInt", Ttype_int) in
  assert (prune named_type == named_type);

  let v1 = fresh_var 0 in
  let v2 = fresh_var 0 in
  assert (prune v1 == v1);
  unify v1 v2;
  assert (prune v1 == prune v2);
  unify v2 Ttype_float;
  assert (prune v1 == Ttype_float)

let%test_unit "type equality test" =
  assert (equal bool_type bool_type);
  assert (not (equal bool_type unit_type));

  assert (equal Ttype_bottom Ttype_bottom);
  assert (equal Ttype_int Ttype_int);
  assert (not (equal Ttype_int Ttype_float));

  assert (equal (Ttype_paren bool_type) bool_type);
  assert (
    equal
      (Ttype_tuple [ bool_type; unit_type ])
      (Ttype_tuple [ bool_type; unit_type ]));
  assert (
    not
      (equal
         (Ttype_tuple [ bool_type; unit_type ])
         (Ttype_tuple [ unit_type; bool_type ])))

let%test_unit "type comparison test" =
  assert (compare bool_type bool_type = 0);
  assert (compare Ttype_bottom Ttype_bottom = 0);
  assert (compare Ttype_int Ttype_int = 0);

  assert (compare bool_type unit_type <> 0);
  assert (compare Ttype_int Ttype_float <> 0);

  assert (compare (Ttype_paren bool_type) bool_type = 0);
  assert (
    compare
      (Ttype_tuple [ bool_type; unit_type ])
      (Ttype_tuple [ bool_type; unit_type ])
    = 0);
  assert (
    compare
      (Ttype_tuple [ bool_type; unit_type ])
      (Ttype_tuple [ unit_type; bool_type ])
    <> 0)

let%test_unit "type comparison test with bottom type" =
  assert (compare Ttype_bottom Ttype_bottom = 0);
  assert (compare Ttype_bottom Ttype_int <> 0);
  assert (compare Ttype_int Ttype_bottom <> 0)

let%test_unit "unification of type variables" =
  let v1 = fresh_var 0 in
  let v2 = fresh_var 0 in
  unify v1 v2;
  assert (equal (prune v1) (prune v2));
  unify v1 Ttype_int;
  assert (equal (prune v1) Ttype_int);
  assert (equal (prune v2) Ttype_int)

let%test_unit "unification of named types" =
  let t1 = Ttype_named (mk_ident "MyInt", Ttype_int) in
  let t2 = Ttype_named (mk_ident "MyInt", Ttype_int) in
  unify t1 t2;
  assert (equal (prune t1) (prune t2))

let%test_unit "unification of recursive type variable" =
  let v1 = fresh_var 0 in
  let t1 = Ttype_tuple [ v1 ] in
  let t2 = Ttype_tuple [ Ttype_tuple [ v1 ] ] in
  let exception_unified = ref false in
  (try unify t1 t2 with UnificationError _ -> exception_unified := true);
  assert !exception_unified

let%test_unit "unification of tuples" =
  let v1 = fresh_var 0 in
  let v2 = fresh_var 0 in
  let t1 = Ttype_tuple [ v1; Ttype_int ] in
  let t2 = Ttype_tuple [ Ttype_float; v2 ] in
  unify t1 t2;
  assert (equal (prune v1) Ttype_float);
  assert (equal (prune v2) Ttype_int)

let%test_unit "is bool like test" =
  assert (is_bool_like bool_type);
  assert (not (is_bool_like unit_type));
  assert (not (is_bool_like Ttype_char));
  assert (is_bool_like Ttype_bottom);
  assert (is_bool_like (Ttype_paren bool_type))

let%test_unit "is bool like test with custom bool type" =
  let fake_bool =
    {
      tdecl_enum_name = { value = "FakeBool"; loc = Location.dummy };
      tdecl_enum_loc = Location.dummy;
      tdecl_enum_constructors = [];
    }
  in

  let fake_on_ctor =
    {
      tdecl_constructor_name = { value = "On"; loc = Location.dummy };
      tdecl_constructor_enum = fake_bool;
      tdecl_constructor_loc = Location.dummy;
    }
  in

  let fake_off_ctor =
    {
      tdecl_constructor_name = { value = "Off"; loc = Location.dummy };
      tdecl_constructor_enum = fake_bool;
      tdecl_constructor_loc = Location.dummy;
    }
  in

  fake_bool.tdecl_enum_constructors <- [ fake_off_ctor; fake_on_ctor ];

  assert (is_bool_like (Ttype_enum fake_bool))

let%test_unit "truthy and falsy constructor test" =
  let false_ctor = falsy_constructor_of bool_type in
  let true_ctor = truthy_constructor_of bool_type in
  assert (false_ctor == false_constructor);
  assert (true_ctor == true_constructor)

let%test_unit "truthy and falsy constructor test with bottom type" =
  (* Bottom type is used for errors, so these functions should try to return
     reasonable defaults for error recovery. *)
  let false_ctor = falsy_constructor_of Ttype_bottom in
  let true_ctor = truthy_constructor_of Ttype_bottom in
  assert (false_ctor == false_constructor);
  assert (true_ctor == true_constructor)

let%test_unit "bool type" =
  match bool_type with
  | Ttype_enum decl ->
      assert (decl == bool_declaration);
      assert (List.length decl.tdecl_enum_constructors = 2);
      let false_ctor = List.hd decl.tdecl_enum_constructors in
      let true_ctor = List.hd (List.tl decl.tdecl_enum_constructors) in
      assert (false_ctor == false_constructor);
      assert (true_ctor == true_constructor)
  | _ -> assert false

let%test_unit "unit type" =
  match unit_type with
  | Ttype_enum decl ->
      assert (decl == unit_declaration);
      assert (List.length decl.tdecl_enum_constructors = 1);
      let unit_ctor = List.hd decl.tdecl_enum_constructors in
      assert (unit_ctor == unit_constructor)
  | _ -> assert false
