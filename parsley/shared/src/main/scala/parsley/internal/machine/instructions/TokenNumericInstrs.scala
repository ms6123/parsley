/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import parsley.token.descriptions.PlusSignPresence

import parsley.internal.deepembedding.Sign.{CombinedType, DoubleType, IntType, SignType}
import parsley.internal.errors.{ExpectItem, ExpectRaw}
import parsley.internal.machine.{Context, InterpreterContext}
import parsley.internal.machine.XAssert.*
import parsley.internal.machine.errors.ExpectedError

private [internal] final class TokenSign(ty: SignType, plusPresence: PlusSignPresence) extends Instr with SpecializedInstr {
    val neg: Any => Any = ty match {
        case IntType => ((x: IntType.resultType) => -x).asInstanceOf[Any => Any]
        case DoubleType => ((x: DoubleType.resultType) => -x).asInstanceOf[Any => Any]
        case CombinedType => ((x: CombinedType.resultType) => x.fold(n => Left(-n), n => Right(-n))).asInstanceOf[Any => Any]
    }
    val pos = (x: Any) => x

    private [this] val expecteds: Set[ExpectItem] =
        if (plusPresence ne PlusSignPresence.Illegal) Set(new ExpectRaw("+"), new ExpectRaw("-"))
        else                                          Set(new ExpectRaw("-"))

    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        // This could be simplified, but the "fail" branches need to be duplicated...
        if (ctx.moreInput && ctx.peekChar == '-') {
            ctx.fastUncheckedConsumeChars(1)
            ctx.push(neg)
            pc + 1
        }
        else if ((plusPresence ne PlusSignPresence.Illegal) && ctx.moreInput && ctx.peekChar == '+') {
            ctx.fastUncheckedConsumeChars(1)
            ctx.push(pos)
            pc + 1
        }
        else if (plusPresence eq PlusSignPresence.Required) {
            ctx.fail(new ExpectedError(ctx.offset, ctx.line, ctx.col, expecteds, 1))
        }
        else {
            ctx.pushError(new ExpectedError(ctx.offset, ctx.line, ctx.col, expecteds, 1))
            ctx.addErrorToHintsAndPop()
            ctx.push(pos)
            pc + 1
        }
    }

    @JitImpl
    def apply(ctx: Context): Any = {
        ensureRegularInstruction(ctx)
        // This could be simplified, but the "fail" branches need to be duplicated...
        if (ctx.moreInput && ctx.peekChar == '-') {
            ctx.fastUncheckedConsumeChars(1)
            neg
        }
        else if ((plusPresence ne PlusSignPresence.Illegal) && ctx.moreInput && ctx.peekChar == '+') {
            ctx.fastUncheckedConsumeChars(1)
            pos
        }
        else if (plusPresence eq PlusSignPresence.Required) {
            ctx.good = false
            FailMarker
        }
        else {
            pos
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = "TokenSign"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz + 1, handlers))
    
    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz + 1, handlers))
}
