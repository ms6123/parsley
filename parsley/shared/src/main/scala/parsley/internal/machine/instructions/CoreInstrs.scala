/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import parsley.XAssert.*

import parsley.internal.machine.{Context, ParseRunner}
import parsley.internal.machine.XAssert.*
import parsley.internal.machine.errors.{EmptyError, EmptyHints}

// Stack Manipulators
private [internal] final class Push[A](x: A) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.pushAndContinue(x)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Push($x)"
    // $COVERAGE-ON$

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}
private [internal] object Push {
    val Unit = new Push(())
}

private [internal] final class Fresh[A](x: =>A) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.pushAndContinue(x)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Fresh($x)"
    // $COVERAGE-ON$

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [internal] object Pop extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.stack.pop_()
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = "Pop"
    // $COVERAGE-ON$

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [internal] object Swap extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        val y = ctx.stack.upop()
        val x = ctx.stack.peekAndExchange(y)
        ctx.unsafePushAndContinue(x)
    }
    // $COVERAGE-OFF$
    override def toString: String = "Swap"
    // $COVERAGE-ON$

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

// Applicative Functors
private [internal] object Apply extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        val x = ctx.stack.upop()
        val f = ctx.stack.peek[Any => Any]
        ctx.exchangeAndContinue(f(x))
    }
    // $COVERAGE-OFF$
    override def toString: String = "Apply"
    // $COVERAGE-ON$

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

// Monadic
private [internal] final class DynCall(f: (Any, Int) => ParseRunner) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        val runner = f(ctx.stack.upop(), ctx.regs.size)
        runner.dynCall(ctx.asInstanceOf[runner.ContextT]) // TODO: avoidable?
    }
    // $COVERAGE-OFF$
    override def toString: String = "DynCall(?)"
    // $COVERAGE-ON$
}
private [internal] object DynCall {
    def apply[A](f: (A, Int) => ParseRunner): DynCall = new DynCall(f.asInstanceOf[(Any, Int) => ParseRunner])
}

// Control Flow
private [internal] object Halt extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.running = false
    }
    // $COVERAGE-OFF$
    override def toString: String = "Halt"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = None

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [internal] final case class Call(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.call(label)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Call($label)"
    // $COVERAGE-ON$

    override def labels: Seq[Int] = Seq.empty
}

private [internal] object Return extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.ret()
    }
    // $COVERAGE-OFF$
    override def toString: String = "Return"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = None

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [internal] final class Empty(width: Int) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.fail(new EmptyError(ctx.offset, ctx.line, ctx.col, unexpectedWidth = width))
    }
    // $COVERAGE-OFF$
    override def toString: String = "Empty"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = None
}
private [internal] object Empty {
    val zero = new Empty(0)
}

private [internal] final class PushHandler(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.pushHandler(label)
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"PushHandler($label)"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = Some(label :: handlers)

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [internal] object PopHandler extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.popHandler()
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = "PopHandler"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = Some(handlers.tail)

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [internal] final class PushHandlerAndClearHints(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.pushHandler(label)
        ctx.hints = EmptyHints
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"PushHandlerAndClearHints($label)"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = Some(label :: handlers)

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [internal] final class PushHandlerAndStateAndClearHints(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.pushHandler(label)
        ctx.saveState()
        ctx.hints = EmptyHints
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"PushHandlerAndStateAndClearHints($label)"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = Some(label :: handlers)

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [internal] final class PushHandlerAndState(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.pushHandler(label)
        ctx.saveState()
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"PushHandlerAndState($label)"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = Some(label :: handlers)

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [internal] object PopHandlerAndState extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.states = ctx.states.tail
        ctx.popHandler()
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = "PopHandlerAndState"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = Some(handlers.tail)

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [internal] final class Jump(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.pc = label
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Jump($label)"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = None

    override def failPath(handlers: List[Int]): Option[List[Int]] = None

    override def jumpPaths(handlers: List[Int]): Seq[(List[Int], Int)] = Seq(handlers -> label)
}

private [internal] final class JumpAndPopCheck(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        // TODO: should this be mergeHints?
        ctx.popHandler()
        ctx.pc = label
    }
    // $COVERAGE-OFF$
    override def toString: String = s"JumpAndPopCheck($label)"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = None

    override def failPath(handlers: List[Int]): Option[List[Int]] = None

    override def jumpPaths(handlers: List[Int]): Seq[(List[Int], Int)] = Seq(handlers.tail -> label)
}

private [internal] final class JumpAndPopState(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.popHandler()
        ctx.states = ctx.states.tail
        ctx.pc = label
    }
    // $COVERAGE-OFF$
    override def toString: String = s"JumpAndPopState($label)"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = None

    override def failPath(handlers: List[Int]): Option[List[Int]] = None

    override def jumpPaths(handlers: List[Int]): Seq[(List[Int], Int)] = Seq(handlers.tail -> label)
}

private [internal] final class Catch(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.restoreHints()
        ctx.catchNoConsumed(ctx.handlers.check) {
            ctx.replaceHandler(label)
            ctx.inc()
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Catch($label)"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = Some(label :: handlers.tail)

    override def failPath(handlers: List[Int]): Option[List[Int]] = Some(handlers.tail)
}

private [internal] final class RestoreAndPushHandler(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.restoreState()
        ctx.restoreHints()
        ctx.good = true
        ctx.replaceHandler(label)
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"RestoreAndPushHandler($label)"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = Some(label :: handlers.tail)

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
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
