/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import parsley.internal.errors.{CaretWidth, RigidCaret, UnexpectDesc}
import parsley.internal.machine.{Context, InterpreterContext}
import parsley.internal.machine.XAssert.*
import parsley.internal.machine.errors.{DefuncError, EmptyError}

private [internal] final class RelabelHints(labels: Iterable[String]) extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        if (ctx.offset == ctx.handlerCheck) ctx.replaceHint(labels)
        // COK
        // do nothing
        ctx.mergeHints()
        ctx.popHandler()
        pc + 1
    }

    @JitImpl
    def apply(): Unit = ()

    // $COVERAGE-OFF$
    override def toString: String = s"RelabelHints($labels)"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] final class RelabelErrorAndFail(labels: Iterable[String]) extends Instr with RefailInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        // this has the effect of relabelling all hints since the start of the label combinator
        ctx.restoreHints()
        ctx.relabelError(labels)
        ctx.popHandler()
        ctx.fail()
    }

    @JitImpl
    def apply(): Unit = ()

    // $COVERAGE-OFF$
    override def toString: String = s"RelabelErrorAndFail($labels)"
    // $COVERAGE-ON$
}

private [internal] object HideHints extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        // according to old label logic, we do this unconditionally
        /*if (ctx.offset == ctx.handlerCheck)*/ ctx.popHints()
        ctx.mergeHints()
        ctx.popHandler()
        pc + 1
    }

    @JitImpl
    def apply(): Unit = ()

    // $COVERAGE-OFF$
    override def toString: String = "HideHints"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

// FIXME: Gigaparsec points out the hints aren't being used here, I believe they should be!
private [internal] object HideErrorAndFail extends Instr with RefailInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.restoreHints()
        if (ctx.offset == ctx.handlerCheck) ctx.hideError()
        ctx.popHandler()
        ctx.fail()
    }

    @JitImpl
    def apply(): Unit = ()

    // $COVERAGE-OFF$
    override def toString: String = "HideErrorAndFail"
    // $COVERAGE-ON$
}

private [internal] object ErrorToHints extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.popHandler()
        ctx.addErrorToHintsAndPop()
        pc + 1
    }

    @JitImpl
    def apply(): Unit = ()

    // $COVERAGE-OFF$
    override def toString: String = "ErrorToHints"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] object MergeErrorsAndFail extends Instr with RefailInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.popHandler()
        ctx.mergeErrors()
        ctx.fail()
    }

    @JitImpl
    def apply(): Unit = ()

    // $COVERAGE-OFF$
    override def toString: String = "MergeErrorsAndFail"
    // $COVERAGE-ON$
}

private [internal] class ApplyReasonAndFail(reason: String) extends Instr with RefailInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.applyReason(reason)
        ctx.popHandler()
        ctx.fail()
    }

    @JitImpl
    def apply(): Unit = ()

    // $COVERAGE-OFF$
    override def toString: String = s"ApplyReasonAndFail($reason)"
    // $COVERAGE-ON$
}

private [internal] class AmendAndFail private (partial: Boolean) extends Instr with RefailInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.restoreHints() //TODO: verify this is ok; it feels more right than the restore on the labelling
        ctx.popHandler()
        ctx.amendError(partial)
        ctx.states = ctx.states.tail
        ctx.fail()
    }

    @JitImpl
    def apply(ctx: Context): Unit = {
        ctx.states = ctx.states.tail
    }

    // $COVERAGE-OFF$
    override def toString: String = "AmendAndFail"
    // $COVERAGE-ON$
}
private [internal] object AmendAndFail {
    private [this] val partial = new AmendAndFail(partial = true)
    private [this] val full = new AmendAndFail(partial = false)
    def apply(partial: Boolean): AmendAndFail = if (partial) this.partial else this.full
}

private [internal] object EntrenchAndFail extends Instr with RefailInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.popHandler()
        ctx.entrenchError()
        ctx.fail()
    }

    @JitImpl
    def apply(): Unit = ()

    // $COVERAGE-OFF$
    override def toString: String = "EntrenchAndFail"
    // $COVERAGE-ON$
}

private [internal] class DislodgeAndFail(n: Int) extends Instr with RefailInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.popHandler()
        ctx.dislodgeError(n)
        ctx.fail()
    }

    @JitImpl
    def apply(): Unit = ()

    // $COVERAGE-OFF$
    override def toString: String = s"DislodgeAndFail($n)"
    // $COVERAGE-ON$
}

private [internal] object SetLexicalAndFail extends Instr with RefailInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.markErrorAsLexical()
        ctx.popHandler()
        ctx.fail()
    }

    @JitImpl
    def apply(): Unit = ()

    // $COVERAGE-OFF$
    override def toString: String = "SetLexicalAndFail"
    // $COVERAGE-ON$
}

private [internal] final class Fail(width: CaretWidth, msgs: String*) extends Instr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.failWithMessage(width, msgs*)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Fail(${msgs.mkString(", ")})"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] final class Unexpected(msg: String, width: CaretWidth) extends Instr {
    private [this] val unexpected = new UnexpectDesc(msg, width)
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.unexpectedFail(None, unexpected)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Unexpected($msg)"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] final class VanillaGen[A](gen: parsley.errors.VanillaGen[A]) extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        // stack will have an (A, Int) pair on it
        val (x, caretWidth) = ctx.stack.pop[(A, Int)]()
        val unex = gen.unexpected(x)
        val reason = gen.reason(x)
        val err = unex.makeError(ctx.offset, ctx.line, ctx.col, gen.adjustWidth(x, caretWidth))
        ctx.fail(err.withReason(reason))
    }
    
    @JitImpl
    def apply(): Unit = ()

    // $COVERAGE-OFF$
    override def toString: String = "VanillaGen"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers))
}

private [internal] final class SpecializedGen[A](gen: parsley.errors.SpecializedGen[A]) extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        // stack will have an (A, Int) pair on it
        val (x, caretWidth) = ctx.stack.pop[(A, Int)]()
        ctx.failWithMessage(new RigidCaret(gen.adjustWidth(x, caretWidth)), gen.messages(x)*)
    }
    
    @JitImpl
    def apply(): Unit = ()

    // $COVERAGE-OFF$
    override def toString: String = "SpecializedGen"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers))
}
