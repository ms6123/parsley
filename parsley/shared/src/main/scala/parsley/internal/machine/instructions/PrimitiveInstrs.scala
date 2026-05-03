/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import parsley.token.errors.LabelConfig

import parsley.internal.errors.ExpectDesc
import parsley.internal.machine.Context
import parsley.internal.machine.XAssert.*

private [internal] final class Satisfies(f: Char => Boolean, expected: Iterable[ExpectDesc]) extends Instr {
    def this(f: Char => Boolean, expected: LabelConfig) = this(f, expected.asExpectDescs)
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        if (ctx.moreInput && f(ctx.peekChar)) {
            ctx.push(ctx.consumeChar())
            pc + 1
        }
        else ctx.expectedFail(expected, unexpectedWidth = 1)
    }
    // $COVERAGE-OFF$
    override def toString: String = "Sat(?(_))"
    // $COVERAGE-ON$
}

private [internal] object RestoreAndFail extends Instr with RefailInstr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.popHandler()
        // Pop input off head then fail to next handler
        ctx.restoreState()
        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = "RestoreAndFail"
    // $COVERAGE-ON$
}

private [internal] object RestoreHintsAndState extends Instr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.restoreHints()
        ctx.restoreState()
        ctx.popHandler()
        pc + 1
    }
    // $COVERAGE-OFF$
    override def toString: String = "RestoreHintsAndState"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = Some(handlers.tail)

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [internal] object PopStateAndFail extends Instr with RefailInstr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.popHandler()
        ctx.states = ctx.states.tail
        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = "PopStateAndFail"
    // $COVERAGE-ON$
}

private [internal] object PopStateRestoreHintsAndFail extends Instr with RefailInstr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.restoreHints()
        ctx.popHandler()
        ctx.states = ctx.states.tail
        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = "PopStateRestoreHintsAndFail"
    // $COVERAGE-ON$
}

// Position Extractors
private [internal] object Line extends Instr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.push(ctx.line)
        pc + 1
    }
    // $COVERAGE-OFF$
    override def toString: String = "Line"
    // $COVERAGE-ON$

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [internal] object Col extends Instr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.push(ctx.col)
        pc + 1
    }
    // $COVERAGE-OFF$
    override def toString: String = "Col"
    // $COVERAGE-ON$

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [internal] object Offset extends Instr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.push(ctx.offset)
        pc + 1
    }
    // $COVERAGE-OFF$
    override def toString: String = "Offset"
    // $COVERAGE-ON$

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

// Register-Manipulators
private [internal] final class Get(reg: Int) extends Instr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.push(ctx.regs(reg))
        pc + 1
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Get(r$reg)"
    // $COVERAGE-ON$

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [internal] final class Put(reg: Int) extends Instr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.writeReg(reg, ctx.stack.upop())
        pc + 1
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Put(r$reg)"
    // $COVERAGE-ON$

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [internal] final class PutAndFail(reg: Int) extends Instr with RefailInstr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.popHandler()
        ctx.writeReg(reg, ctx.stack.upeek)
        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"PutAndFail(r$reg)"
    // $COVERAGE-ON$
}

private [internal] object Span extends Instr {
    override def apply(ctx: Context, pc: Int): Int = {
        // this uses the state stack because post #132 we will need a save point to obtain the start of the input
        ensureRegularInstruction(ctx)
        val startOffset = ctx.states.offset
        ctx.states = ctx.states.tail
        ctx.popHandler()
        ctx.push(ctx.input.substring(startOffset, ctx.offset))
        pc + 1
    }
    // $COVERAGE-OFF$
    override def toString: String = "Span"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = Some(handlers.tail)

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [parsley] final class ExpandRefs(newSz: Int) extends Instr {
    override def apply(ctx: Context, pc: Int): Int = {
        if (newSz > ctx.regs.size) {
            ctx.regs = java.util.Arrays.copyOf(ctx.regs, newSz)
        }
        pc + 1
    }

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}
