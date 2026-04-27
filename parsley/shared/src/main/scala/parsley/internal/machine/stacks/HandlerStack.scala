/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.stacks

import parsley.internal.machine.errors.DefuncHints

private [machine] abstract class HandlerStack[SelfT <: HandlerStack[SelfT]](
    val stacksz: Int,
    var check: Int,
    val hints: DefuncHints,
    val hintOffset: Int) {
    val tail: SelfT
    
    def pc: Int
    def pc_=(v: Int): Unit
}

private [machine] object HandlerStack {
    // $COVERAGE-OFF$
    implicit def inst[HandlerStackT <: HandlerStack[HandlerStackT]]: Stack[HandlerStackT] = HandlerStackCompanion()
    // $COVERAGE-ON$
}

private class HandlerStackCompanion[HandlerStackT <: HandlerStack[HandlerStackT]] extends Stack[HandlerStackT] {
    type ElemTy = (Int, Int)

    // $COVERAGE-OFF$
    // TODO: needs to change
    override protected def show(x: ElemTy): String = {
        val (pc, stacksz) = x
        s"Handler:$pc(-${stacksz + 1})"
    }

    override protected def head(xs: HandlerStackT): ElemTy = (xs.pc, xs.stacksz)

    override protected def tail(xs: HandlerStackT): HandlerStackT = xs.tail
    // $COVERAGE-ON$
}