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

type 'a tree = Node of 'a * 'a tree list

(** [print_tree print_val tree] prints the given [tree] to the standard OCaml
    formatter. [print_val] is a function that prints a node's value of type
    ['a]. *)
let print_tree print_val print_val_cleanup fmt (tree : 'a tree) =
  let rec print_subtree prefix is_last (Node (value, children)) =
    let connector = if is_last then "`-" else "|-" in
    Format.fprintf fmt "%s%s%a@," prefix connector print_val value;

    let child_prefix = prefix ^ if is_last then "  " else "| " in

    let rec iter_children children =
      match children with
      | [] -> () (* Base case: no more children *)
      | [ last_child ] ->
          (* Last child case: print with is_last = true *)
          print_subtree child_prefix true last_child
      | head_child :: tail_children ->
          (* Not the last child: print with is_last = false *)
          print_subtree child_prefix false head_child;
          (* Recurse on the rest of the children *)
          iter_children tail_children
    in

    iter_children children;
    print_val_cleanup value
  in

  match tree with
  | Node (value, children) ->
      Format.fprintf fmt "@[<v>";
      print_val fmt value;
      Format.fprintf fmt "@,";

      let rec iter_children children =
        match children with
        | [] -> ()
        | [ last_child ] -> print_subtree "" true last_child
        | head_child :: tail_children ->
            print_subtree "" false head_child;
            iter_children tail_children
      in

      iter_children children;
      print_val_cleanup value;

      Format.fprintf fmt "@]@."
