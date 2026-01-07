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

(* ========================================================
 * MiniSync Intermediate Representation (IR) definition
 * The IR represents the sequential code (as in imperative languages)
 * after translation from the normalized AST.
 *)

open Tast

type imemory = { imem_id : int; imem_type : Type.t }
and ivariable = { ivar_id : int; ivar_type : Type.t }
and iinstance = { iinst_id : int; iinst_node : inode }

and iexpression_kind =
  | Iexpr_skip
  | Iexpr_const of Nast.nconstant
  | Iexpr_var of ivariable
  | Iexpr_func_ref of inode
  | Iexpr_state of imemory
  | Iexpr_tuple of iexpression list
  | Iexpr_seq of iexpression list
  | Iexpr_binary of binop * iexpression * iexpression
  | Iexpr_unary of unop * iexpression
  | Iexpr_call of iexpression * iexpression list
  | Iexpr_match of iexpression * (constructor_declaration * iexpression) list
  | Iexpr_assign of ivariable * iexpression
  | Iexpr_state_assign of imemory * iexpression
  | Iexpr_instance_reset of iinstance
  | Iexpr_instance_step of ivariable list * iinstance * iexpression list

and iexpression = { iexpr_kind : iexpression_kind; iexpr_type : Type.t }

and inode = {
  inode_nast : Nast.nnode;
  inode_params : ivariable list;
      (** Parameters of the node. These variables are provided as inputs to the
          [step] method of the node. *)
  inode_return_type : Type.t;
      (** Return type of the node. This is the type returned by the [step]
          method of the node. *)
  mutable inode_memories : imemory list;
      (** All variables that need to be stored accross steps by this node. *)
  mutable inode_instances : iinstance list;
      (** All referenced instances that need to be stored by this node. *)
  mutable inode_variables : ivariable list;
      (** All local variables used by this node. These variables do not need to
          be stored accross steps. *)
  mutable inode_reset : iexpression list;
      (** Statements for the [reset] methods of the node. These statements do
          not depend on the parameters of the node. This is code used for
          initialization and resets. *)
  mutable inode_step : iexpression list;
      (** Statements for the [step] methods of the node. These statements may
          refer to the parameters of the node. *)
  mutable inode_var_cpt : int;
      (** Counter for generating fresh local variables in this node. *)
  mutable inode_mem_cpt : int;
      (** Counter for generating fresh memory variables in this node. *)
  mutable inode_inst_cpt : int;
      (** Counter for generating fresh instance variables in this node. *)
}

let mk_iexpr kind ty = { iexpr_kind = kind; iexpr_type = ty }
let mk_unit_iexpr kind = mk_iexpr kind Type.unit_type

module Variable = struct
  type t = ivariable

  let equal t1 t2 = t1.ivar_id = t2.ivar_id
  let compare t1 t2 = Stdlib.compare t1.ivar_id t2.ivar_id
end

module VariableSet = Set.Make (Variable)

module Memory = struct
  type t = imemory

  let equal t1 t2 = t1.imem_id = t2.imem_id
  let compare t1 t2 = Stdlib.compare t1.imem_id t2.imem_id
end

module MemorySet = Set.Make (Memory)

module Instance = struct
  type t = iinstance

  let equal t1 t2 = t1.iinst_id = t2.iinst_id
  let compare t1 t2 = Stdlib.compare t1.iinst_id t2.iinst_id
end

module InstanceSet = Set.Make (Instance)
