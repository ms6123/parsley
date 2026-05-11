/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import scala.annotation.unused

import parsley.XAssert.*

import parsley.internal.machine.{Context, InterpreterContext, ParseRunner}
import parsley.internal.machine.XAssert.*
import parsley.internal.machine.errors.{EmptyError, EmptyHints}

// Stack Manipulators
private [internal] final class Push[A](x: A) extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.push(x)
        pc + 1
    }

    @JitImpl
    def apply: Any = x

    // $COVERAGE-OFF$
    override def toString: String = s"Push($x)"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz + 1, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}
private [internal] object Push {
    val Unit = new Push(())
}

private [internal] final class Fresh[A](x: =>A) extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.push(x)
        pc + 1
    }

    @JitImpl
    def apply: Any = x

    // $COVERAGE-OFF$
    override def toString: String = s"Fresh($x)"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz + 1, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] object Pop extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.stack.pop_()
        pc + 1
    }

    @JitImpl(consumeOperands = 1)
    def apply(@unused operand: Any): Unit = ()

    // $COVERAGE-OFF$
    override def toString: String = "Pop"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz - 1, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] object Swap extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val y = ctx.stack.upop()
        val x = ctx.stack.peekAndExchange(y)
        ctx.unsafePush(x)
        pc + 1
    }
    // $COVERAGE-OFF$
    override def toString: String = "Swap"
    // $COVERAGE-ON$

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

// Applicative Functors
private [internal] object Apply extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val x = ctx.stack.upop()
        val f = ctx.stack.peek[Any => Any]
        ctx.exchange(f(x))
        pc + 1
    }

    @JitImpl(consumeOperands = 2)
    def apply(f: Any, x: Any): Any = {
        f.asInstanceOf[Any => Any](x)
    }

    // $COVERAGE-OFF$
    override def toString: String = "Apply"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz - 1, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

// Monadic
private [internal] final class DynCall(f: (Any, Int, Boolean) => ParseRunner) extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val runner = f(ctx.stack.upop(), ctx.regs.length, false)
        runner.dynCall(ctx, pc).asInstanceOf[Int]
    }

    @JitImpl(consumeOperands = 1)
    def apply(x: Any, ctx: Context): Any = {
        val runner = f(x, ctx.regs.length, true)
        runner.dynCall(ctx, -1)
    }

    // $COVERAGE-OFF$
    override def toString: String = "DynCall(?)"
    // $COVERAGE-ON$
}
private [internal] object DynCall {
    def apply[A](f: (A, Int, Boolean) => ParseRunner): DynCall = new DynCall(f.asInstanceOf[(Any, Int, Boolean) => ParseRunner])
}

// Control Flow
private [internal] object Halt extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.running = false
        pc
    }
    // $COVERAGE-OFF$
    override def toString: String = "Halt"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] final case class Call(var label: Int, producesResults: Boolean) extends InstrWithLabel with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.call(label)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Call($label)"
    // $COVERAGE-ON$

    override def copy: Instr = Call(label, producesResults)

    override def labels: Seq[Int] = Seq.empty

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(if (producesResults) stacksz + 1 else stacksz, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(if (producesResults) stacksz + 1 else stacksz, handlers))
}

private [internal] object Return extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.ret()
    }
    // $COVERAGE-OFF$
    override def toString: String = "Return"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] final class Empty(width: Int) extends Instr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.fail(new EmptyError(ctx.offset, ctx.line, ctx.col, unexpectedWidth = width))
    }
    // $COVERAGE-OFF$
    override def toString: String = "Empty"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}
private [internal] object Empty {
    val zero = new Empty(0)
}

private [internal] final class PushHandler(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.pushHandler(label)
        pc + 1
    }
    // $COVERAGE-OFF$
    override def toString: String = s"PushHandler($label)"
    // $COVERAGE-ON$

    override def copy: Instr = PushHandler(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, HandlerInfo(label, stacksz) :: handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] object PopHandler extends Instr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.popHandler()
        pc + 1
    }
    // $COVERAGE-OFF$
    override def toString: String = "PopHandler"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] final class PushHandlerAndClearHints(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.pushHandler(label)
        ctx.clearHints()
        pc + 1
    }
    // $COVERAGE-OFF$
    override def toString: String = s"PushHandlerAndClearHints($label)"
    // $COVERAGE-ON$

    override def copy: Instr = PushHandlerAndClearHints(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, HandlerInfo(label, stacksz) :: handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] final class PushHandlerAndStateAndClearHints(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.pushHandler(label)
        ctx.saveState()
        ctx.clearHints()
        pc + 1
    }
    // $COVERAGE-OFF$
    override def toString: String = s"PushHandlerAndStateAndClearHints($label)"
    // $COVERAGE-ON$

    override def copy: Instr = PushHandlerAndStateAndClearHints(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, HandlerInfo(label, stacksz) :: handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] final class PushHandlerAndState(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.pushHandler(label)
        ctx.saveState()
        pc + 1
    }
    // $COVERAGE-OFF$
    override def toString: String = s"PushHandlerAndState($label)"
    // $COVERAGE-ON$

    override def copy: Instr = PushHandlerAndState(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, HandlerInfo(label, stacksz) :: handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] object PopHandlerAndState extends Instr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.states = ctx.states.tail
        ctx.popHandler()
        pc + 1
    }
    // $COVERAGE-OFF$
    override def toString: String = "PopHandlerAndState"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] final class Jump(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        label
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Jump($label)"
    // $COVERAGE-ON$

    override def copy: Instr = Jump(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def jumpPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq(label -> StackInfo(stacksz, handlers))
}

private [internal] final class JumpAndPopCheck(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        // TODO: should this be mergeHints?
        ctx.popHandler()
        label
    }
    // $COVERAGE-OFF$
    override def toString: String = s"JumpAndPopCheck($label)"
    // $COVERAGE-ON$

    override def copy: Instr = JumpAndPopCheck(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def jumpPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq(label -> StackInfo(stacksz, handlers.tail))
}

private [internal] final class JumpAndPopState(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.popHandler()
        ctx.states = ctx.states.tail
        label
    }
    // $COVERAGE-OFF$
    override def toString: String = s"JumpAndPopState($label)"
    // $COVERAGE-ON$

    override def copy: Instr = JumpAndPopState(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def jumpPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq(label -> StackInfo(stacksz, handlers.tail))
}

private [internal] final class Catch(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.restoreHints()
        ctx.catchNoConsumed(ctx.handlerCheck) {
            ctx.replaceHandler(label)
            pc + 1
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Catch($label)"
    // $COVERAGE-ON$

    override def copy: Instr = Catch(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, HandlerInfo(label, stacksz) :: handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))
}

private [internal] final class RestoreAndPushHandler(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.restoreState()
        ctx.restoreHints()
        ctx.good = true
        ctx.replaceHandler(label)
        pc + 1
    }
    // $COVERAGE-OFF$
    override def toString: String = s"RestoreAndPushHandler($label)"
    // $COVERAGE-ON$

    override def copy: Instr = RestoreAndPushHandler(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, HandlerInfo(label, stacksz) :: handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

/*private [internal] object Refail extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.handlers = ctx.handlers.tail
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = "Refail"
    // $COVERAGE-ON$
}*/
