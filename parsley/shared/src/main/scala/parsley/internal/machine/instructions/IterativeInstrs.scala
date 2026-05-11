/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import scala.annotation.unused
import scala.collection.mutable

import parsley.internal.machine.{Context, InterpreterContext}
import parsley.internal.machine.XAssert.*

private [internal] final class ManyJump(var label: Int) extends InstrWithLabel with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val x = ctx.stack.upop()
        ctx.stack.peek[mutable.Builder[Any, Any]] += x
        ctx.updateCheckOffset()
        label
    }

    @JitImpl(consumeOperands = 2)
    def apply(builder: Any, x: Any, ctx: Context): Any = {
        builder.asInstanceOf[mutable.Builder[Any, Any]] += x
        ctx.updateCheckOffset()
        builder
    }

    // $COVERAGE-OFF$
    override def toString: String = s"ManyJump($label)"
    // $COVERAGE-ON$

    override def copy: Instr = ManyJump(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def jumpPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq(label -> StackInfo(stacksz - 1, handlers))
}

private [internal] object ManyHandler extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        ctx.catchNoConsumed(ctx.handlerCheck) {
            ctx.popHandler()
            ctx.addErrorToHintsAndPop()
            ctx.exchange(ctx.stack.peek[mutable.Builder[Any, Any]].result())
            pc + 1
        }
    }

    @JitImpl(consumeOperands = 1)
    def apply(builder: Any, ctx: Context): Any = {
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        val check = ctx.handlerCheck
        ctx.popHandler()
        if (ctx.offset == check) {
            builder.asInstanceOf[mutable.Builder[Any, Any]].result()
        } else FailMarker
    }

    // $COVERAGE-OFF$
    override def toString: String = "ManyHandler"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))
}

// TODO: Factor these handlers out!
private [internal] final class SkipManyJump(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.updateCheckOffset()
        label
    }
    // $COVERAGE-OFF$
    override def toString: String = s"SkipManyJump($label)"
    // $COVERAGE-ON$

    override def copy: Instr = SkipManyJump(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def jumpPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq(label -> StackInfo(stacksz, handlers))
}

private [internal] final class ChainPostJump(var label: Int) extends InstrWithLabel with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val op = ctx.stack.pop[Any => Any]()
        ctx.stack.exchange(op(ctx.stack.upeek))
        ctx.updateCheckOffset()
        label
    }

    @JitImpl(consumeOperands = 2)
    def apply(x: Any, op: Any, ctx: Context): Any = {
        ctx.updateCheckOffset()
        op.asInstanceOf[Any => Any](x)
    }

    // $COVERAGE-OFF$
    override def toString: String = s"ChainPostJump($label)"
    // $COVERAGE-ON$

    override def copy: Instr = ChainPostJump(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def jumpPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq(label -> StackInfo(stacksz - 1, handlers))
}

private [internal] object IterativeHandler extends Instr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        ctx.catchNoConsumed(ctx.handlerCheck) {
            ctx.popHandler()
            ctx.addErrorToHintsAndPop()
            pc + 1
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = "IterativeHandler"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))
}

private final class AndThen[-A, B, +C](f: A => B, g: B => C) extends (A => C) {
    final def apply(x: A): C = g match {
        case g: AndThen[_, _, _] => g(f(x))
        case g                   => g(f(x))
    }
}

private [internal] final class ChainPreJump(var label: Int) extends InstrWithLabel with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val f = ctx.stack.pop[Any => Any]()
        ctx.stack.exchange(new AndThen(f, ctx.stack.peek[Any => Any]))
        ctx.updateCheckOffset()
        label
    }

    @JitImpl(consumeOperands = 2)
    def apply(g: Any, f: Any, ctx: Context): Any = {
        ctx.updateCheckOffset()
        new AndThen(f.asInstanceOf[Any => Any], g.asInstanceOf[Any => Any])
    }

    // $COVERAGE-OFF$
    override def toString: String = s"ChainPreJump($label)"
    // $COVERAGE-ON$

    override def copy: Instr = ChainPreJump(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def jumpPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq(label -> StackInfo(stacksz - 1, handlers))
}

private [internal] final class ChainlJump(var label: Int) extends InstrWithLabel with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val y = ctx.stack.upop()
        val op = ctx.stack.pop[(Any, Any) => Any]()
        ctx.stack.exchange(op(ctx.stack.peek[Any], y))
        ctx.updateCheckOffset()
        label
    }

    @JitImpl(consumeOperands = 3)
    def apply(x: Any, op: Any, y: Any, ctx: Context): Any = {
        ctx.updateCheckOffset()
        op.asInstanceOf[(Any, Any) => Any](x, y)
    }

    // $COVERAGE-OFF$
    override def toString: String = s"Chainl($label)"
    // $COVERAGE-ON$

    override def copy: Instr = ChainlJump(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def jumpPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq(label -> StackInfo(stacksz - 2, handlers))
}

private [internal] final class ROps(val op: (Any, Any) => Any, val lx: Any, val rest: ROps)
private [internal] object ROps {
    val empty: ROps = null
    private [instructions] def reduce(ops: ROps, rx: Any): Any = {
        if (ops eq empty) rx
        else reduce(ops.rest, ops.op(ops.lx, rx))
    }
}

private [internal] final class ChainrJump(var label: Int) extends InstrWithLabel with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val f = ctx.stack.pop[(Any, Any) => Any]()
        val x = ctx.stack.upop()
        ctx.stack.exchange(new ROps(f, x, ctx.stack.peek[ROps]))
        ctx.popHandler()
        label
    }

    @JitImpl(consumeOperands = 3)
    def apply(rops: Any, x: Any, f: Any, ctx: Context): Any = {
        ctx.popHandler()
        new ROps(f.asInstanceOf, x, rops.asInstanceOf)
    }

    // $COVERAGE-OFF$
    override def toString: String = s"ChainrJump($label)"
    // $COVERAGE-ON$

    override def copy: Instr = ChainrJump(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def jumpPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq(label -> StackInfo(stacksz - 2, handlers.tail))
}

private [internal] final class ChainrOpHandler(wrap: Any => Any) extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        ctx.catchNoConsumed(ctx.handlerCheck) {
            ctx.popHandler()
            ctx.addErrorToHintsAndPop()
            val y = ctx.stack.upop()
            ctx.exchange(ROps.reduce(ctx.stack.peek[ROps], wrap(y)))
            pc + 1
        }
    }

    @JitImpl(consumeOperands = 2)
    def apply(rops: Any, y: Any, ctx: Context): Any = {
        val check = ctx.handlerCheck
        ctx.popHandler()
        if (ctx.offset == check) {
            ROps.reduce(rops.asInstanceOf, wrap(y))
        } else FailMarker
    }

    // $COVERAGE-OFF$
    override def toString: String = "ChainrOpHandler"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz - 1, handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz - 1, handlers.tail))
}
private [internal] object ChainrOpHandler  {
    def apply[A, B](wrap: A => B): ChainrOpHandler = new ChainrOpHandler(wrap.asInstanceOf[Any => Any])
}

private [internal] final class SepEndBy1Jump(var label: Int) extends InstrWithLabel with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val x = ctx.stack.upop()
        ctx.stack.pop_() // the bool
        ctx.stack.peek[mutable.Builder[Any, Any]] += x
        ctx.stack.upush(true)
        // pop second handler and jump
        ctx.popHandler()
        ctx.updateCheckOffset()
        label
    }

    @JitImpl(consumeOperands = 3, afterActions = Array(JitImpl.Action.PushTrue))
    def apply(builder: Any, @unused bool: Any, x: Any, ctx: Context): Any = {
        builder.asInstanceOf[mutable.Builder[Any, Any]] += x
        // pop second handler and jump
        ctx.popHandler()
        ctx.updateCheckOffset()
        builder
    }

    // $COVERAGE-OFF$
    override def toString: String = s"SepEndBy1Jump($label)"
    // $COVERAGE-ON$

    override def copy: Instr = SepEndBy1Jump(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def jumpPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq(label -> StackInfo(stacksz - 1, handlers.tail))
}

private [instructions] object SepEndBy1Handlers {
    def pushAccWhenCheckValidAndContinue(ctx: InterpreterContext, label: Int, check: Int, acc: mutable.Builder[Any, Any], readP: Boolean): Int = {
        if (ctx.offset != check || !readP) ctx.fail()
        else {
            ctx.addErrorToHintsAndPop()
            ctx.good = true
            ctx.exchange(acc.result())
            label
        }
    }

    def accWhenCheckValidOrFailMarker(ctx: Context, check: Int, acc: mutable.Builder[Any, Any], readP: Boolean): Any = {
        if (ctx.offset != check || !readP) {
            // Fail
            FailMarker
        }
        else {
            acc.result()
        }
    }
}

private [internal] class SepEndBy1SepHandler(var label: Int) extends InstrWithLabel with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        val check = ctx.handlers.check
        ctx.handlers = ctx.handlers.tail
        // p succeeded and sep didn't, so push p and fall-through to the whole handler
        val x = ctx.stack.upop()
        ctx.stack.pop_() // the bool is no longer needed
        val acc = ctx.stack.peek[mutable.Builder[Any, Any]]
        acc += x
        // discard the other handler and increment so that we are sat on the other handler
        assert(ctx.instrs(ctx.pc + 1) eq SepEndBy1WholeHandler, "the next instruction from the sep handler must be the whole handler")
        assert(ctx.handlers.pc == ctx.pc + 1, "the top-most handler must be the whole handler in the sep handler")
        ctx.handlers = ctx.handlers.tail
        SepEndBy1Handlers.pushAccWhenCheckValidAndContinue(ctx, label, check, acc, readP = true)
    }

    @JitImpl(consumeOperands = 3)
    def apply(builder: Any, @unused bool: Any, x: Any, ctx: Context): Any = {
        val check = ctx.handlerCheck
        ctx.popHandler()

        builder.asInstanceOf[mutable.Builder[Any, Any]] += x

        ctx.popHandler()

        SepEndBy1Handlers.accWhenCheckValidOrFailMarker(ctx, check, builder.asInstanceOf, readP = true)
    }

    override def copy: Instr = SepEndBy1SepHandler(label)

    // $COVERAGE-OFF$
    override def toString: String = "SepEndBy1SepHandler"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz - 2, handlers.tail.tail))

    override def jumpPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq(
        label -> StackInfo(stacksz - 2, handlers.tail.tail),
    )
}

private [internal] object SepEndBy1WholeHandler extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        val check = ctx.handlerCheck
        ctx.popHandler()
        val readP = ctx.stack.pop[Boolean]()
        SepEndBy1Handlers.pushAccWhenCheckValidAndContinue(ctx, pc + 1, check, ctx.stack.peek[mutable.Builder[Any, Any]], readP)
    }

    @JitImpl(consumeOperands = 2)
    def apply(builder: Any, readP: Any, ctx: Context): Any = {
        val check = ctx.handlerCheck
        ctx.popHandler()

        SepEndBy1Handlers.accWhenCheckValidOrFailMarker(ctx, check, builder.asInstanceOf, readP.asInstanceOf)
    }

    // $COVERAGE-OFF$
    override def toString: String = "SepEndBy1WholeHandler"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz - 1, handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz - 1, handlers.tail))
}

private [internal] final case class ManyUntil(var label: Int) extends InstrWithLabel with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.stack.upop() match {
            case ManyUntil.Stop => 
                ctx.exchange(ctx.stack.peek[mutable.Builder[Any, Any]].result())
                pc + 1
            case x =>
                ctx.stack.peek[mutable.Builder[Any, Any]] += x
                label
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"ManyUntil($label)"
    // $COVERAGE-ON$

    override def copy: Instr = ManyUntil(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz - 1, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def jumpPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq(label -> StackInfo(stacksz - 1, handlers))
}
private [parsley] object ManyUntil {
    object Stop
}

private [internal] final class SkipManyUntil(var label: Int) extends InstrWithLabel with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.stack.upop() match {
            case ManyUntil.Stop => pc + 1
            case _ => label
        }
    }

    @JitImpl(consumeOperands = 1)
    def apply(x: Any, ctx: Context, pc: Int): Int = {
        x match {
            case ManyUntil.Stop => pc + 1
            case _ => label
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = s"SkipManyUntil($label)"
    // $COVERAGE-ON$

    override def copy: Instr = SkipManyUntil(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz - 1, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def jumpPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq(label -> StackInfo(stacksz - 1, handlers))
}
