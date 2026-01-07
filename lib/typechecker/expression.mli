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

type t = Tast.texpression

val equal : t -> t -> bool
(** [equal e1 e2] returns true if the two expressions [e1] and [e2] are
    syntactically equivalent. Notably, it ignores parentheses. *)

val fold : ?ignore_fby:bool -> (t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold ?ignore_fby f expr acc] folds over the expression [expr] with the
    function [f] and the initial accumulator [acc]. The function [f] is applied
    to each node of the expression tree in a pre-order traversal.

    If [ignore_fby] is set to [true], the fold will ignore the right-hand side
    of [fby] expressions and [pre] unary expressions. This is useful for
    analyses that want to consider only the instantaneous part of the
    expression. *)
