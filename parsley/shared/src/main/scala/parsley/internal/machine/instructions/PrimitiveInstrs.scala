/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import parsley.token.errors.LabelConfig

import parsley.internal.errors.ExpectDesc
import parsley.internal.machine.{Context, InterpreterContext}
import parsley.internal.machine.XAssert.*

private [internal] final class Satisfies(val f: Char => Boolean, expected: Iterable[ExpectDesc]) extends Instr with SpecializedInstr {
    def this(f: Char => Boolean, expected: LabelConfig) = this(f, expected.asExpectDescs)
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        if (ctx.moreInput && f(ctx.peekChar)) {
            ctx.push(ctx.consumeChar())
            pc + 1
        }
        else ctx.expectedFail(expected, unexpectedWidth = 1)
    }

    @JitImpl(constants = Array("f"), intReturnKind = JitImpl.IntKind.Char)
    def apply(f: Char => Boolean, ctx: Context): Int = {
        if (ctx.moreInput && f(ctx.peekChar)) {
            ctx.consumeChar()
        }
        else {
            -1
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = "Sat(?(_))"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz + 1, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz + 1, handlers))
}

private [internal] object RestoreAndFail extends Instr with RefailInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.popHandler()
        // Pop input off head then fail to next handler
        ctx.restoreState()
        ctx.fail()
    }
    
    @JitImpl
    def apply(ctx: Context): Unit = {
        ctx.restoreState()
    }
    
    // $COVERAGE-OFF$
    override def toString: String = "RestoreAndFail"
    // $COVERAGE-ON$
}

private [internal] object RestoreHintsAndState extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.restoreHints()
        ctx.restoreState()
        ctx.popHandler()
        pc + 1
    }

    @JitImpl
    def apply(ctx: Context): Unit = {
        ctx.restoreState()
    }
    
    // $COVERAGE-OFF$
    override def toString: String = "RestoreHintsAndState"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] object PopStateAndFail extends Instr with RefailInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.popHandler()
        ctx.states = ctx.states.tail
        ctx.fail()
    }

    @JitImpl
    def apply(ctx: Context): Unit = {
        ctx.states = ctx.states.tail
    }
    
    // $COVERAGE-OFF$
    override def toString: String = "PopStateAndFail"
    // $COVERAGE-ON$
}

private [internal] object PopStateRestoreHintsAndFail extends Instr with RefailInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.restoreHints()
        ctx.popHandler()
        ctx.states = ctx.states.tail
        ctx.fail()
    }
    
    @JitImpl
    def apply(ctx: Context): Unit = {
        ctx.states = ctx.states.tail
    }
    
    // $COVERAGE-OFF$
    override def toString: String = "PopStateRestoreHintsAndFail"
    // $COVERAGE-ON$
}

// Position Extractors
private [internal] object Line extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.push(ctx.line)
        pc + 1
    }

    @JitImpl
    def apply(ctx: Context): Any = {
        ctx.line
    }
    
    // $COVERAGE-OFF$
    override def toString: String = "Line"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz + 1, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] object Col extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.push(ctx.col)
        pc + 1
    }

    @JitImpl
    def apply(ctx: Context): Any = {
        ctx.col
    }
    
    // $COVERAGE-OFF$
    override def toString: String = "Col"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz + 1, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] object Offset extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.push(ctx.offset)
        pc + 1
    }
    
    @JitImpl
    def apply(ctx: Context): Any = {
        ctx.offset
    }
    
    // $COVERAGE-OFF$
    override def toString: String = "Offset"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz + 1, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

// Register-Manipulators
private [internal] final class Get(reg: Int) extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.push(ctx.regs(reg))
        pc + 1
    }

    @JitImpl
    def apply(ctx: Context): Any = {
        ctx.regs(reg)
    }

    // $COVERAGE-OFF$
    override def toString: String = s"Get(r$reg)"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz + 1, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] final class Put(reg: Int) extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.writeReg(reg, ctx.stack.upop())
        pc + 1
    }

    @JitImpl(consumeOperands = 1)
    def apply(x: Any, ctx: Context): Unit = {
        ctx.writeReg(reg, x)
    }

    // $COVERAGE-OFF$
    override def toString: String = s"Put(r$reg)"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz - 1, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] final class PutAndFail(reg: Int) extends Instr with SpecializedInstr with RefailInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.popHandler()
        ctx.writeReg(reg, ctx.stack.upeek)
        ctx.fail()
    }

    @JitImpl(consumeOperands = 1)
    def apply(x: Any, ctx: Context): Unit = {
        ctx.writeReg(reg, x)
    }

    // $COVERAGE-OFF$
    override def toString: String = s"PutAndFail(r$reg)"
    // $COVERAGE-ON$

    override def failStacksz(stacksz: Int): Int = stacksz - 1
}

private [internal] object Span extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        // this uses the state stack because post #132 we will need a save point to obtain the start of the input
        ensureRegularInstruction(ctx)
        val startOffset = ctx.states.offset
        ctx.states = ctx.states.tail
        ctx.popHandler()
        ctx.push(ctx.input.substring(startOffset, ctx.offset))
        pc + 1
    }

    @JitImpl
    def apply(ctx: Context): Any = {
        // this uses the state stack because post #132 we will need a save point to obtain the start of the input
        val startOffset = ctx.states.offset
        ctx.states = ctx.states.tail
        ctx.input.substring(startOffset, ctx.offset)
    }

    // $COVERAGE-OFF$
    override def toString: String = "Span"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz + 1, handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [parsley] final class ExpandRefs(newSz: Int) extends Instr {
    override def apply(ctx: Context, pc: Int): Int = {
        if (newSz > ctx.regs.size) {
            ctx.regs = java.util.Arrays.copyOf(ctx.regs, newSz)
        }
        pc + 1
    }

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}
