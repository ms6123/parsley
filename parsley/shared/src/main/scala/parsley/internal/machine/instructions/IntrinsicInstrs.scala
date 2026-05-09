/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import scala.annotation.tailrec

import parsley.XAssert.*

import parsley.errors
import parsley.token.errors.LabelConfig

import parsley.internal.errors.{EndOfInput, ExpectDesc, ExpectItem}
import parsley.internal.machine.{Context, InterpreterContext}
import parsley.internal.machine.XAssert.*
import parsley.internal.errors.RigidCaret
import parsley.internal.machine.errors.ClassicFancyError

private [internal] final class Lift2(f: (Any, Any) => Any) extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val y = ctx.stack.upop()
        ctx.exchange(f(ctx.stack.upeek, y))
        pc + 1
    }

    @JitImpl(consumeOperands = 2)
    def apply(x: Any, y: Any, ctx: Context): Any = {
        ensureRegularInstruction(ctx)
        f(x, y)
    }

    // $COVERAGE-OFF$
    override def toString: String = "Lift2(f)"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz - 1, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}
private [internal] object Lift2 {
    def apply[A, B, C](f: (A, B) => C): Lift2 = new Lift2(f.asInstanceOf[(Any, Any) => Any])
}

private [internal] final class Lift3(f: (Any, Any, Any) => Any) extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val z = ctx.stack.upop()
        val y = ctx.stack.upop()
        ctx.exchange(f(ctx.stack.upeek, y, z))
        pc + 1
    }

    @JitImpl(consumeOperands = 3)
    def apply(x: Any, y: Any, z: Any, ctx: Context): Any = {
        ensureRegularInstruction(ctx)
        f(x, y, z)
    }

    // $COVERAGE-OFF$
    override def toString: String = "Lift3(f)"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz - 2, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}
private [internal] object Lift3 {
    def apply[A, B, C, D](f: (A, B, C) => D): Lift3 = new Lift3(f.asInstanceOf[(Any, Any, Any) => Any])
}

private [internal] class CharTok private (c: Char, errorItem: Iterable[ExpectItem]) extends Instr {
    def this(c: Char, expected: LabelConfig) = this(c, expected.asExpectItems(s"$c"))
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        if (ctx.moreInput && ctx.peekChar == c) {
            ctx.consumeChar()
            pc + 1
        }
        else ctx.expectedFail(errorItem, unexpectedWidth = 1)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Chr($c)"
    // $COVERAGE-ON$
}

private [internal] class SupplementaryCharTok private (codepoint: Int, errorItem: Iterable[ExpectItem]) extends Instr {
    def this(codepoint: Int, expected: LabelConfig) = this(codepoint, expected.asExpectItems(Character.toChars(codepoint).mkString))

    assert(Character.isSupplementaryCodePoint(codepoint), "SupplementaryCharTok should only be used for supplementary code points")
    val h = Character.highSurrogate(codepoint)
    val l = Character.lowSurrogate(codepoint)
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        if (ctx.moreInput(2) && ctx.peekChar(0) == h && ctx.peekChar(1) == l) {
            ctx.fastConsumeSupplementaryChar()
            pc + 1
        }
        else ctx.expectedFail(errorItem, unexpectedWidth = 1)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"SupplementaryChr($h$l)"
    // $COVERAGE-ON$
}

private [internal] final class StringTok private (s: String, errorItem: Iterable[ExpectItem]) extends Instr {
    def this(s: String, expected: LabelConfig) = this(s, expected.asExpectItems(s))

    private [this] val sz = s.length
    private [this] val codePointLength = s.codePointCount(0, sz)

    @tailrec private [this] def compute(i: Int, lineAdjust: Int, colAdjust: StringTok.Adjust): (Int => Int, Int => Int) = {
        if (i < sz) {
            val (partialLineAdjust, partialColAdjust) = build(lineAdjust, colAdjust)
            partialLineAdjusters(i) = partialLineAdjust
            partialColAdjusters(i) = partialColAdjust
            s.charAt(i) match {
                case '\n' => compute(i + 1, lineAdjust + 1, new StringTok.Set)
                case '\t' => compute(i + 1, lineAdjust, colAdjust.tab)
                case _    => colAdjust.next(); compute(i + 1, lineAdjust, colAdjust)
            }
        }
        else build(lineAdjust, colAdjust)
    }
    private [this] def build(lineAdjust: Int, colAdjust: StringTok.Adjust): (Int => Int, Int => Int) = {
        (if (lineAdjust == 0) line => line else _ + lineAdjust, colAdjust.toAdjuster)
    }
    private [this] val partialLineAdjusters = new Array[Int => Int](sz)
    private [this] val partialColAdjusters = new Array[Int => Int](sz)
    private [this] val (lineAdjust, colAdjust) = compute(0, 0, new StringTok.Offset)

    @tailrec private def go(ctx: Context, pc: Int, i: Int, j: Int): Int = {
        if (j < sz && i < ctx.inputsz && ctx.input.charAt(i) == s.charAt(j)) go(ctx, pc, i + 1, j + 1)
        else if (j < sz) {
            // The offset, line and column haven't been edited yet, so are in the right place
            // FIXME: this might be a more appropriate way of capping off the demand for the error?
            val newPc = ctx.expectedFail(errorItem, /*s.codePointCount(0, j+1)*/codePointLength)
            ctx.offset = i
            // These help maintain a consistent internal state, this makes the debuggers
            // output less confusing in the string case in particular.
            ctx.col = partialColAdjusters(j)(ctx.col)
            ctx.line = partialLineAdjusters(j)(ctx.line)
            newPc
        }
        else {
            ctx.col = colAdjust(ctx.col)
            ctx.line = lineAdjust(ctx.line)
            ctx.offset = i
            pc + 1
        }
    }

    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        go(ctx, pc, ctx.offset, 0)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Str($s)"
    // $COVERAGE-ON$
}

private [internal] final class UniSat(f: Int => Boolean, expected: Iterable[ExpectDesc]) extends Instr with SpecializedInstr {
    def this(f: Int => Boolean, expected: LabelConfig) = this(f, expected.asExpectDescs)
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        lazy val hc = ctx.peekChar(0)
        lazy val h = hc.toInt
        lazy val l = ctx.peekChar(1)
        lazy val c = Character.toCodePoint(hc, l)
        if (ctx.moreInput(2) && hc.isHighSurrogate && Character.isSurrogatePair(hc, l) && f(c)) {
            ctx.fastConsumeSupplementaryChar()
            ctx.push(c)
            pc + 1
        }
        else if (ctx.moreInput && f(h)) {
            ctx.updatePos(hc)
            ctx.offset += 1
            ctx.push(h)
            pc + 1
        }
        else ctx.expectedFail(expected, unexpectedWidth = 1)
    }
    // $COVERAGE-OFF$
    override def toString: String = "UniSat(?(_))"
    // $COVERAGE-ON$

    @JitImpl
    def apply(ctx: Context): Any = {
        ensureRegularInstruction(ctx)
        lazy val hc = ctx.peekChar(0)
        lazy val h = hc.toInt
        lazy val l = ctx.peekChar(1)
        lazy val c = Character.toCodePoint(hc, l)
        if (ctx.moreInput(2) && hc.isHighSurrogate && Character.isSurrogatePair(hc, l) && f(c)) {
            ctx.fastConsumeSupplementaryChar()
            c
        }
        else if (ctx.moreInput && f(h)) {
            ctx.updatePos(hc)
            ctx.offset += 1
            h
        }
        else {
            ctx.good = false
            FailMarker
        }
    }

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz + 1, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz + 1, handlers))
}

private [internal] final class If(var label: Int) extends InstrWithLabel with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        if (ctx.stack.pop[Boolean]()) label
        else pc + 1
    }

    @JitImpl(consumeOperands = 1)
    def apply(condition: Any, ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        if (condition.asInstanceOf[Boolean]) label
        else pc + 1
    }

    // $COVERAGE-OFF$
    override def toString: String = s"If(true: $label)"
    // $COVERAGE-ON$

    override def copy: Instr = If(label)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz - 1, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def jumpPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq(label -> StackInfo(stacksz - 1, handlers))
}

private [internal] final case class Case(var label: Int) extends InstrWithLabel with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.stack.peek[Either[_, _]] match {
            case Left(x)  =>
                ctx.stack.exchange(x)
                label
            case Right(y) =>
                ctx.exchange(y)
                pc + 1
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Case(left: $label)"
    // $COVERAGE-ON$

    override def copy: Instr = Case(label)

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def jumpPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq(label -> StackInfo(stacksz, handlers))
}

private [internal] object NegLookFail extends Instr with RefailInstr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val reached = ctx.offset
        // Recover the previous state; notFollowedBy NEVER consumes input
        ctx.restoreState()
        ctx.restoreHints()
        // A previous success is a failure
        ctx.popHandler()
        ctx.expectedFail(None, reached - ctx.offset)
    }
    // $COVERAGE-OFF$
    override def toString: String = "NegLookFail"
    // $COVERAGE-ON$
}

private [internal] object NegLookGood extends Instr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureHandlerInstruction(ctx)
        // Recover the previous state; notFollowedBy NEVER consumes input
        ctx.restoreState()
        ctx.restoreHints()
        ctx.popHandler()
        // A failure is what we wanted
        ctx.good = true
        ctx.popError()
        pc + 1
    }
    // $COVERAGE-OFF$
    override def toString: String = "NegLookGood"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [internal] object Eof extends Instr {
    private [this] final val expected = Some(EndOfInput)
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        if (ctx.offset == ctx.inputsz) pc + 1
        else ctx.expectedFail(expected, unexpectedWidth = 1)
    }
    // $COVERAGE-OFF$
    override final def toString: String = "Eof"
    // $COVERAGE-ON$
}

private [internal] final class Modify(reg: Int, f: Any => Any) extends Instr {
    override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.writeReg(reg, f(ctx.regs(reg)))
        pc + 1
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Modify($reg, f)"
    // $COVERAGE-ON$

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}
private [internal] object Modify {
    def apply[S](reg: Int, f: S => S): Modify = new Modify(reg, f.asInstanceOf[Any => Any])
}

private [internal] final class SwapAndPut(reg: Int) extends Instr with SpecializedInstr {
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        ctx.writeReg(reg, ctx.stack.peekAndExchange(ctx.stack.upop()))
        pc + 1
    }

    @JitImpl(consumeOperands = 2)
    def apply(x: Any, y: Any, ctx: Context): Any = {
        ensureRegularInstruction(ctx)
        ctx.writeReg(reg, x)
        y
    }

    // $COVERAGE-OFF$
    override def toString: String = s"SwapAndPut(r$reg)"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz - 1, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None
}

private [instructions] abstract class FilterLike extends Instr with SpecializedInstr {
    var good: Int
    var bad: Int

    final override def relabel(labels: Int => Int): this.type = {
        good = labels(good)
        bad = labels(bad)
        this
    }

    final def carryOn(ctx: Context): Unit = {
        ctx.states = ctx.states.tail
        ctx.popHandler()
    }

    final def fail(ctx: Context): Unit = {
        ctx.replaceHandler(bad)
    }

    final override def labels: Seq[Int] = Seq(good, bad)

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, HandlerInfo(bad, stacksz - 1) :: handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = None

    override def jumpPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq(good -> StackInfo(stacksz, handlers.tail))
}

private [internal] final class Filter[A](_pred: A => Boolean, var good: Int, var bad: Int) extends FilterLike {
    private [this] val pred = _pred.asInstanceOf[Any => Boolean]

    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val x = ctx.stack.upeek
        if (pred(x)) {
            carryOn(ctx)
            good
        }
        else {
            fail(ctx)
            ctx.exchange((x, ctx.offset - ctx.states.offset))
            pc + 1
        }
    }

    @JitImpl(consumeOperands = 1)
    def apply(x: Any, ctx: Context): Any = {
        ensureRegularInstruction(ctx)
        if (pred(x)) {
            carryOn(ctx)
            x
        }
        else {
            fail(ctx)
            FallthroughMarker
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = s"Filter(???, good = $good)"
    // $COVERAGE-ON$

    override def copy: Instr = Filter(pred, good, bad)
}

private [internal] final class MapFilter[A, B](_pred: A => Option[B], var good: Int, var bad: Int) extends FilterLike {
    private [this] val pred = _pred.asInstanceOf[Any => Option[B]]

    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val x = ctx.stack.upeek
        val opt = pred(x)
        // Sorry, it's faster :(
        if (opt.isDefined) {
            ctx.stack.exchange(opt.get)
            carryOn(ctx)
            good
        }
        else {
            fail(ctx)
            ctx.exchange((x, ctx.offset - ctx.states.offset))
            pc + 1
        }
    }

    @JitImpl(consumeOperands = 1)
    def apply(x: Any, ctx: Context): Any = {
        ensureRegularInstruction(ctx)
        val opt = pred(x)
        if (opt.isDefined) {
            carryOn(ctx)
            opt.get
        }
        else {
            fail(ctx)
            FallthroughMarker
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = s"MapFilter(???, good = $good)"
    // $COVERAGE-ON$

    override def copy: Instr = MapFilter(pred, good, bad)
}

private [internal] final class FilterPartialVanilla[A](f: PartialFunction[A, (errors.VanillaGen.UnexpectedItem, Option[String])]) extends Instr with SpecializedInstr {
    private [this] val pred = f.asInstanceOf[PartialFunction[Any, (errors.VanillaGen.UnexpectedItem, Option[String])]]

    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val x = ctx.stack.upeek
        val state = ctx.states
        ctx.states = state.tail
        ctx.popHandler()
        pred.applyOrElse(x, FilterPartial.orNull) match {
            case null => pc + 1
            case (unex, reason) =>
                val caretWidth = ctx.offset - state.offset
                val err = unex.makeError(state.offset, state.line, state.col, caretWidth)
                ctx.fail(err.withReason(reason))
        }
    }

    @JitImpl(consumeOperands = 1)
    def apply(x: Any, ctx: Context): Any = {
        ensureRegularInstruction(ctx)
        val state = ctx.states
        ctx.states = state.tail
        ctx.popHandler()
        pred.applyOrElse(x, FilterPartial.orNull) match {
            case null => x
            case _ =>
                ctx.good = false
                FailMarker
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = s"FilterPartialVanilla(?)"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))
}

private [internal] final class FilterPartialSpecialized[A, B](f: A => Either[Seq[String], B]) extends Instr with SpecializedInstr {
    private [this] val pred = f.asInstanceOf[Any => Either[Seq[String], Any]]

    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val x = ctx.stack.upeek
        val state = ctx.states
        ctx.states = state.tail
        ctx.popHandler()
        pred(x) match {
            case Right(y) =>
                ctx.exchange(y)
                pc + 1
            case Left(msgs) =>
                val caretWidth = ctx.offset - state.offset
                ctx.fail(new ClassicFancyError(state.offset, state.line, state.col, new RigidCaret(caretWidth), msgs*))
        }
    }

    @JitImpl(consumeOperands = 1)
    def apply(x: Any, ctx: Context): Any = {
        ensureRegularInstruction(ctx)
        val state = ctx.states
        ctx.states = state.tail
        ctx.popHandler()
        pred(x) match {
            case Right(y) =>
                y
            case Left(_) =>
                ctx.good = false
                FailMarker
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = s"FilterPartialSpecialized(?)"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers.tail))
}

private [instructions] object FilterPartial {
    val orNull = (_: Any) => null
}

// Companion Objects
private [internal] object StringTok {
    private [StringTok] abstract class Adjust {
        private [StringTok] def tab: Adjust
        private [StringTok] def next(): Unit
        private [StringTok] def toAdjuster: Int => Int
    }
    // A line has been read, so any updates are fixed
    private [StringTok] class Set extends Adjust {
        private [this] var at = 1
        // Round up to the nearest multiple of 4 /+1/
        private [StringTok] def tab = { at = ((at + 3) & -4) | 1; this } // scalastyle:ignore magic.number
        private [StringTok] def next() = at += 1
        private [StringTok] def toAdjuster = {
            val x = at // capture it now, so it doesn't need to hold the object later
            _ => x
        }
    }
    // No information about alignment: a line or a tab hasn't been read
    private [StringTok] class Offset extends Adjust {
        private [this] var by = 0
        private [StringTok] def tab = new OffsetAlignOffset(by)
        private [StringTok] def next() = by += 1
        private [StringTok] def toAdjuster = {
            val x = by // capture it now, so it doesn't need to hold the object later
            if (x == 0) col => col else _ + x
        }
    }
    // A tab was read, and no lines, so we adjust first, then align, and work with an aligned value
    private [StringTok] class OffsetAlignOffset(firstBy: Int) extends Adjust {
        private [this] var thenBy = 0
        // Round up to nearest multiple of /4/ (offset from aligned, not real value)
        private [StringTok] def tab = { thenBy = (thenBy | 3) + 1; this }
        private [StringTok] def next() = thenBy += 1
        // Round up to the nearest multiple of 4 /+1/
        private [StringTok] def toAdjuster = {
            val x = firstBy // provide an indirection to the object
            val y = thenBy  // capture it now, so it doesn't need to hold the object later
            col => (((col + x + 3) & -4) | 1) + y // scalastyle:ignore magic.number
        }
    }
}
