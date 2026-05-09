/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import parsley.XAssert.*

import parsley.internal.machine.{Context, InterpreterContext}
import parsley.internal.machine.stacks.ArrayStack
import scala.annotation.tailrec

import parsley.internal.machine.XAssert.{ensureHandlerInstruction, ensureRegularInstruction}

private [internal] sealed abstract class ShuntToken {
    private [instructions] def handle(ctx: Context, state: ShuntingYardState, shunt: ShuntJump): Int
}

private [internal] final class Atom(val v: Any, val lvl: Int) extends ShuntToken {
    private [instructions] def handle(ctx: Context, state: ShuntingYardState, shunt: ShuntJump): Int = {
        state.atoms.push(this)
        shunt.gotoPostInfix(ctx, state)
    }
}
private [internal] abstract class Operator extends ShuntToken {
    private [instructions] val prec: Int
    private [instructions] def isPostfix: Boolean
    private [instructions] def isInfixNonAssoc: Boolean
    private [instructions] def reduce(state: ShuntingYardState, shunt: ShuntInstr): Unit

    @tailrec
    private [instructions] final def reduceWhilePrecGreater(state: ShuntingYardState, shunt: ShuntInstr): Unit = {
        if (state.operators.nonEmpty && state.operators.peek.prec.compare(prec) > 0) {
            state.operators.pop[Operator]().reduce(state, shunt)
            reduceWhilePrecGreater(state, shunt)
        }
    }

    @tailrec
    private [instructions] final def reduceWhilePrecGreaterOrEqual(state: ShuntingYardState, shunt: ShuntInstr): Unit = {
        if (state.operators.nonEmpty && state.operators.peek.prec.compare(prec) >= 0) {
            state.operators.pop[Operator]().reduce(state, shunt)
            reduceWhilePrecGreaterOrEqual(state, shunt)
        }
    }

}

private [internal] final class PrefixOp(f: Any => Any, val prec: Int) extends Operator {
    private [instructions] def isPostfix: Boolean = false
    private [instructions] def isInfixNonAssoc: Boolean = false
    private [instructions] def handle(ctx: Context, state: ShuntingYardState, shunt: ShuntJump): Int = {
        if (state.operators.nonEmpty && prec.compare(state.operators.peek.prec) < 0) {
            // This is a malformed expression
            ctx.popHandler()
            val width = shunt.restoreStateGetWidth(ctx)
            ctx.expectedFail(Nil, width)
        } else {
            state.operators.push(this)
            shunt.gotoPreAtom(ctx, state)
        }
    }
    private [instructions] def reduce(state: ShuntingYardState, shunt: ShuntInstr): Unit = {
        val input = state.atoms.peek[Atom]
        state.atoms.exchange(new Atom(f(shunt.wrap(input.lvl, prec, input.v)), prec))
    }
}

private [internal] final class PostfixOp(f: Any => Any, val prec: Int) extends Operator {
    private [instructions] def isPostfix: Boolean = true
    private [instructions] def isInfixNonAssoc: Boolean = false
    private [instructions] def handle(ctx: Context, state: ShuntingYardState, shunt: ShuntJump): Int = {
        if (state.operators.nonEmpty && state.operators.peek.isPostfix && prec.compare(state.operators.peek.prec) > 0) {
            // This was an unexpected postfix operator
            ctx.popHandler()
            ctx.restoreState()
            shunt.produceResult(state)
            shunt.endLabel
        } else {
            reduceWhilePrecGreaterOrEqual(state, shunt)
            state.operators.push(this)
            shunt.gotoPostInfix(ctx, state)
        }
    }
    private [instructions] def reduce(state: ShuntingYardState, shunt: ShuntInstr): Unit = {
        val input = state.atoms.peek[Atom]
        state.atoms.exchange(new Atom(f(shunt.wrap(input.lvl, prec, input.v)), prec))
    }
}

private [internal] final class InfixLOp(f: (Any, Any) => Any, val prec: Int) extends Operator {
    private [instructions] def isPostfix: Boolean = false
    private [instructions] def isInfixNonAssoc: Boolean = false
    private [instructions] def handle(ctx: Context, state: ShuntingYardState, shunt: ShuntJump): Int = {
        reduceWhilePrecGreaterOrEqual(state, shunt)
        state.operators.push(this)
        shunt.gotoPreAtom(ctx, state)
    }
    private [instructions] def reduce(state: ShuntingYardState, shunt: ShuntInstr): Unit = {
        val right = state.atoms.pop[Atom]()
        val left = state.atoms.peek[Atom]
        state.atoms.exchange(new Atom(f(shunt.wrap(left.lvl, prec, left.v), shunt.wrap(right.lvl, prec + 1, right.v)), prec))
    }
}

private [internal] final class InfixROp(f: (Any, Any) => Any, val prec: Int) extends Operator {
    private [instructions] def isPostfix: Boolean = false
    private [instructions] def isInfixNonAssoc: Boolean = false
    private [instructions] def handle(ctx: Context, state: ShuntingYardState, shunt: ShuntJump): Int = {
        reduceWhilePrecGreater(state, shunt)
        state.operators.push(this)
        shunt.gotoPreAtom(ctx, state)
    }
    private [instructions] def reduce(state: ShuntingYardState, shunt: ShuntInstr): Unit = {
        val right = state.atoms.pop[Atom]()
        val left = state.atoms.peek[Atom]
        state.atoms.exchange(new Atom(f(shunt.wrap(left.lvl, prec + 1, left.v), shunt.wrap(right.lvl, prec, right.v)), prec))
    }
}

private [internal] final class InfixNOp(f: (Any, Any) => Any, val prec: Int) extends Operator {
    private [instructions] def isPostfix: Boolean = false
    private [instructions] def isInfixNonAssoc: Boolean = true
    private [instructions] def handle(ctx: Context, state: ShuntingYardState, shunt: ShuntJump): Int = {
        reduceWhilePrecGreater(state, shunt)
        if (state.operators.nonEmpty && state.operators.peek.isInfixNonAssoc && state.operators.peek.prec == prec) {
            // This is a special case in which non-associative operators are chained
            ctx.popHandler()
            val width = shunt.restoreStateGetWidth(ctx)
            ctx.expectedFailWithReason(Nil, "operator cannot be applied in sequence as it is non-associative", width)
        } else {
            state.operators.push(this)
            shunt.gotoPreAtom(ctx, state)
        }
    }
    private [instructions] def reduce(state: ShuntingYardState, shunt: ShuntInstr): Unit = {
        val right = state.atoms.pop[Atom]()
        val left = state.atoms.peek[Atom]
        state.atoms.exchange(new Atom(f(shunt.wrap(left.lvl, prec + 1, left.v), shunt.wrap(right.lvl, prec + 1, right.v)), prec))
    }
}

private [instructions] final class ShuntingYardState(
    val atoms: ArrayStack[Atom],
    val operators: ArrayStack[Operator],
    var failOnNoConsumed: Boolean
)

private [internal] object ShuntingYardState {
    def empty = new ShuntingYardState(new ArrayStack(), new ArrayStack(), true)
}

private [instructions] sealed abstract class ShuntInstr(wraps: Array[Array[Any => Any]]) extends Instr with SpecializedInstr {
    private [instructions] final def produceResult(state: ShuntingYardState): Any = {
        reduceAll(state)

        assume(state.atoms.size == 1, "Expected exactly one atom at the end of reduction")

        val atom = state.atoms.peek[Atom]
        wrap(atom.lvl, 0, atom.v)
    }

    @tailrec
    private final def reduceAll(state: ShuntingYardState): Unit = if (state.operators.nonEmpty) {
        state.operators.pop[Operator]().reduce(state, this)
        reduceAll(state)
    }

    private[instructions] final def wrap(from: Int, to: Int, input: Any): Any = {
        assume(to <= from, "Target level must be less than or equal to current level")
        wraps(from)(to) match {
            // case _: =:=[_, _] => input // Would be faster for 2.12 (which would wrap currently). Slower for other versions.
            case _: <:<[_, _] => input // TODO: not tested?
            case wrap => wrap(input)
        }
    }
}

private [internal] final class ShuntJump(var prefixAtomLabel: Int, var postfixInfixLabel: Int, var endLabel: Int, wraps: Array[Array[Any => Any]]) extends ShuntInstr(wraps) {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val token = ctx.stack.pop[ShuntToken]()
        val state = ctx.stack.peek[ShuntingYardState]
        ctx.updateCheckOffset()

        token.handle(ctx, state, this)
    }

    @JitImpl(consumeOperands = 2, beforeActions = Array(JitImpl.Action.Swap, JitImpl.Action.DupX1))
    def apply(token: Any, state: Any, ctx: Context): Int = {
        ensureRegularInstruction(ctx)
        ctx.updateCheckOffset()

        token.asInstanceOf[ShuntToken].handle(ctx, state.asInstanceOf, this)
    }

    private [instructions] final def gotoPreAtom(ctx: Context, state: ShuntingYardState): Int = {
        state.failOnNoConsumed = true
        ctx.refreshState()
        prefixAtomLabel
    }

    private [instructions] final def gotoPostInfix(ctx: Context, state: ShuntingYardState): Int = {
        state.failOnNoConsumed = false
        ctx.refreshState()
        postfixInfixLabel
    }

    private [instructions] final def restoreStateGetWidth(ctx: Context): Int = {
        val currentOffset = ctx.offset
        ctx.restoreState()
        currentOffset - ctx.offset
    }

    override def relabel(labels: Int => Int): this.type = {
        prefixAtomLabel = labels(prefixAtomLabel)
        postfixInfixLabel = labels(postfixInfixLabel)
        endLabel = labels(endLabel)
        this
    }

    override def copy: Instr = ShuntJump(prefixAtomLabel, postfixInfixLabel, endLabel, wraps)

    override def labels: Seq[Int] = Seq(prefixAtomLabel, postfixInfixLabel, endLabel)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz - 1, handlers.tail))

    override def jumpPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq(
        prefixAtomLabel -> StackInfo(stacksz - 1, handlers),
        postfixInfixLabel -> StackInfo(stacksz - 1, handlers),
        endLabel -> StackInfo(stacksz - 1, handlers.tail),
    )

    override def toString: String = s"Shunt(Prefix/Atom: $prefixAtomLabel, Postfix/Infix: $postfixInfixLabel)"
}

private [internal] final class ShuntHandler(wraps: Array[Array[Any => Any]]) extends ShuntInstr(wraps) {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        val handlerCheck = ctx.handlers.check
        ctx.popHandler()
        ctx.states = ctx.states.tail
        if (ctx.offset != handlerCheck || ctx.stack.peek[ShuntingYardState].failOnNoConsumed) {
            // consumed input and/or prefix/atom choice did not match, hard failure
            ctx.fail()
        } else {
            // The end of the expression has been reached
            ctx.good = true
            ctx.addErrorToHintsAndPop()
            ctx.exchange(produceResult(ctx.stack.peek[ShuntingYardState]))
            pc + 1
        }
    }

    @JitImpl(consumeOperands = 1)
    def apply(stateIn: Any, ctx: Context): Any = {
        ensureHandlerInstruction(ctx)
        val state = stateIn.asInstanceOf[ShuntingYardState]
        val handlerCheck = ctx.handlers.check
        ctx.popHandler()
        ctx.states = ctx.states.tail
        if (ctx.offset != handlerCheck || state.failOnNoConsumed) {
            // consumed input and/or prefix/atom choice did not match, hard failure
            FailMarker
        } else {
            // The end of the expression has been reached
            ctx.good = true
            produceResult(state)
        }
    }

    override def copy: Instr = ShuntHandler(wraps)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))

    override def toString: String = s"ShuntHandler"
}
