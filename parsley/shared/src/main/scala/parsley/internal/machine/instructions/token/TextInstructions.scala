/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions.token

import scala.annotation.tailrec

import parsley.character.{isHexDigit, isOctDigit}
import parsley.token.errors.SpecializedFilterConfig

import parsley.internal.collection.immutable.Trie
import parsley.internal.errors.{ExpectDesc, ExpectItem, ExpectRaw}
import parsley.internal.machine.{Context, InterpreterContext}
import parsley.internal.machine.XAssert.*
import parsley.internal.machine.errors.{EmptyError, ExpectedError}
import parsley.internal.machine.instructions.{FailMarker, HandlerInfo, Instr, JitImpl, SpecializedInstr, StackInfo}

private [internal] final class EscapeMapped(escTrie: Trie[Int], caretWidth: Int, expecteds: Set[ExpectItem]) extends Instr with SpecializedInstr {
    def this(escTrie: Trie[Int], escs: Set[String]) = this(escTrie, escs.view.map(_.length).max, escs.map(new ExpectRaw(_)))
    // Do not consume input on failure, it's possible another escape sequence might share a lead
    override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        val res = findFirst(ctx, 0, escTrie)
        if (res >= 0) {
            ctx.push(res)
            pc + 1
        } else {
            ctx.fail()
        }
    }
    
    @JitImpl
    def apply(ctx: Context): Any = {
        ensureRegularInstruction(ctx)
        val res = findFirst(ctx, 0, escTrie)
        if (res >= 0) {
            res
        } else {
            ctx.good = false
            FailMarker
        }
    }

    @tailrec private def findLongest(ctx: Context, off: Int, escs: Trie[Int], longestChar: Int, longestSz: Int): Int = {
        val (nextLongestChar, nextLongestSz) = escs.get("") match {
            case Some(x) => (x, off)
            case None => (longestChar, longestSz)
        }
        lazy val escsNew = escs.suffixes(ctx.peekChar(off))
        if (ctx.moreInput(off + 1) && escsNew.nonEmpty) findLongest(ctx, off + 1, escsNew, nextLongestChar, nextLongestSz)
        else {
            ctx.fastUncheckedConsumeChars(nextLongestSz)
            nextLongestChar
        }
    }

    @tailrec private def findFirst(ctx: Context, off: Int, escs: Trie[Int]): Int = {
        lazy val escsNew = escs.suffixes(ctx.peekChar(off))
        val couldTryMore = ctx.moreInput(off + 1) && escsNew.nonEmpty
        escs.get("") match {
            case Some(x) if couldTryMore => findLongest(ctx, off + 1, escsNew, x, off)
            case Some(x) =>
                ctx.fastUncheckedConsumeChars(off)
                x
            case None if couldTryMore => findFirst(ctx, off + 1, escsNew)
            case None =>
                ctx.fail(new ExpectedError(ctx.offset, ctx.line, ctx.col, expecteds, caretWidth))
                -1
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = "EscapeMapped"
    // $COVERAGE-ON$

    override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz + 1, handlers))

    override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz + 1, handlers))
}

private [machine] abstract class EscapeSomeNumber(radix: Int) extends Instr with SpecializedInstr {
    final def someNumber(ctx: Context, n: Int): EscapeSomeNumber.Result = {
        assume(n > 0, "n cannot be zero for EscapeAtMost or EscapeExactly")
        if (ctx.moreInput && pred(ctx.peekChar)) go(ctx, n - 1, ctx.consumeChar().asDigit) else EscapeSomeNumber.NoDigits
    }

    private def go(ctx: Context, n: Int, num: BigInt): EscapeSomeNumber.Result = {
        if (n > 0 && ctx.moreInput && pred(ctx.peekChar)) go(ctx, n - 1, num * radix + ctx.consumeChar().asDigit)
        else if (n > 0) EscapeSomeNumber.NoMoreDigits(n, num)
        else EscapeSomeNumber.Good(num)
    }

    protected val expected: Some[ExpectDesc] = radix match {
        case 10 => Some(new ExpectDesc("digit"))
        case 16 => Some(new ExpectDesc("hexadecimal digit"))
        case 8 => Some(new ExpectDesc("octal digit"))
        case 2 => Some(new ExpectDesc("bit"))
    }
    
    final protected val expectedSet = expected.toSet[ExpectItem]

    protected val pred: Char => Boolean = radix match {
        case 10 => _.isDigit
        case 16 => isHexDigit(_)
        case 8 => isOctDigit(_)
        case 2 => c => c == '0' || c == '1'
    }

    final override def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz + 1, handlers))

    final override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz + 1, handlers))
}
private [token] object EscapeSomeNumber {
    sealed abstract class Result
    case class Good(num: BigInt) extends Result
    case class NoMoreDigits(remaining: Int, num: BigInt) extends Result
    case object NoDigits extends Result
}

private [internal] final class EscapeAtMost(n: Int, radix: Int) extends EscapeSomeNumber(radix) {
    override def apply(ctx: InterpreterContext, pc: Int): Int = someNumber(ctx, n) match {
        case EscapeSomeNumber.Good(num) =>
            assume(new EmptyError(ctx.offset, ctx.line, ctx.col, 0).isExpectedEmpty, "empty errors don't have expecteds, so don't effect hints")
            ctx.push(num)
            pc + 1
        case EscapeSomeNumber.NoDigits => ctx.expectedFail(expected, unexpectedWidth = 1)
        case EscapeSomeNumber.NoMoreDigits(_, num) =>
            ctx.addHints(expectedSet, unexpectedWidth = 1)
            ctx.push(num)
            pc + 1
    }
    
    @JitImpl
    def apply(ctx: Context): Any = someNumber(ctx, n) match {
        case EscapeSomeNumber.Good(num) =>
            assume(new EmptyError(ctx.offset, ctx.line, ctx.col, 0).isExpectedEmpty, "empty errors don't have expecteds, so don't effect hints")
            num
        case EscapeSomeNumber.NoDigits =>
            ctx.good = false
            FailMarker
        case EscapeSomeNumber.NoMoreDigits(_, num) =>
            num
    }

    // $COVERAGE-OFF$
    override def toString: String = s"EscapeAtMost(n = $n, radix = $radix)"
    // $COVERAGE-ON$
}

private [internal] final class EscapeOneOfExactly(radix: Int, ns: List[Int], inexactErr: SpecializedFilterConfig[Int]) extends EscapeSomeNumber(radix) {
    private val (m :: ms) = ns: @unchecked
    def apply(ctx: InterpreterContext, pc: Int): Int = {
        val origOff = ctx.offset
        val origLine = ctx.line
        val origCol = ctx.col
        someNumber(ctx, m) match {
            case EscapeSomeNumber.Good(num) =>
                assume(new EmptyError(ctx.offset, ctx.line, ctx.col, 0).isExpectedEmpty, "empty errors don't have expecteds, so don't effect hints")
                ctx.push(go(ctx, m, ms, num))
                pc + 1
            case EscapeSomeNumber.NoDigits => ctx.expectedFail(expected, unexpectedWidth = 1)
            case EscapeSomeNumber.NoMoreDigits(remaining, _) =>
                assume(remaining != 0, "cannot be left with 0 remaining digits and failed")
                ctx.fail(inexactErr.mkError(origOff, origLine, origCol, ctx.offset - origOff, m - remaining))
        }
    }

    @JitImpl
    def apply(ctx: Context): Any = {
        val origOff = ctx.offset
        val origLine = ctx.line
        val origCol = ctx.col
        someNumber(ctx, m) match {
            case EscapeSomeNumber.Good(num) =>
                assume(new EmptyError(ctx.offset, ctx.line, ctx.col, 0).isExpectedEmpty, "empty errors don't have expecteds, so don't effect hints")
                go(ctx, m, ms, num)
            case EscapeSomeNumber.NoDigits =>
                ctx.good = false
                FailMarker
            case EscapeSomeNumber.NoMoreDigits(remaining, _) =>
                assume(remaining != 0, "cannot be left with 0 remaining digits and failed")
                ctx.good = false
                FailMarker
        }
    }

    private def rollback(ctx: Context, origOff: Int, origLine: Int, origCol: Int) = {
        // To Cosmin: this can use the save point mechanism you have
        ctx.offset = origOff
        ctx.line = origLine
        ctx.col = origCol
    }

    @tailrec def go(ctx: Context, m: Int, ns: List[Int], acc: BigInt): BigInt = ns match {
        case Nil => acc
        case n :: ns =>
            val origOff = ctx.offset
            val origLine = ctx.line
            val origCol = ctx.col
            someNumber(ctx, n-m) match { // this is the only place where the failure can actually happen: go never fails
                case EscapeSomeNumber.Good(num) =>
                    assume(new EmptyError(ctx.offset, ctx.line, ctx.col, 0).isExpectedEmpty, "empty errors don't have expecteds, so don't effect hints")
                    go(ctx, n, ns, acc * BigInt(radix).pow(n-m) + num)
                case EscapeSomeNumber.NoDigits =>
                    ctx.addHints(expectedSet, unexpectedWidth = 1)
                    rollback(ctx, origOff, origLine, origCol)
                    acc
                case EscapeSomeNumber.NoMoreDigits(remaining, _) =>
                    assume(remaining != 0, "cannot be left with 0 remaining digits and failed")
                    assume(inexactErr.mkError(origOff, origLine, origCol, ctx.offset - origOff, n - remaining).isExpectedEmpty,
                           "filter errors don't have expecteds, so don't effect hints")
                    rollback(ctx, origOff, origLine, origCol)
                    acc
            }
    }

    // $COVERAGE-OFF$
    override def toString: String = s"EscapeOneOfExactly(ns = $ns, radix = $radix)"
    // $COVERAGE-ON$
}
