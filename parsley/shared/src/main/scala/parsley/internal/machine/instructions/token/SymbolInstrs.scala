/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions.token

import scala.annotation.tailrec

import parsley.token.errors.LabelWithExplainConfig
import parsley.token.CharPred

import parsley.internal.collection.immutable.Trie
import parsley.internal.errors.{ExpectDesc, ExpectItem}
import parsley.internal.machine.Context
import parsley.internal.machine.XAssert.*
import parsley.internal.machine.instructions.Instr
import parsley.internal.machine.instructions.token.Specific.computePosInfo

private [token] abstract class Specific extends Instr {
    protected val specific: String
    protected val caseSensitive: Boolean
    protected val expected: Iterable[ExpectItem]
    protected val reason: Option[String]
    private [this] final val strsz = specific.length
    private [this] final val numCodePoints = specific.codePointCount(0, strsz)
    private [this] final val (numNewlines, charsPreFirstTab, numTabs, charsAfterLastTab) = computePosInfo(specific)

    protected def postprocess(ctx: Context): Boolean

    final override def apply(ctx: Context, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        if (ctx.input.regionMatches(!caseSensitive, ctx.offset, specific, 0, strsz)) {
            val oldOffset = ctx.offset
            val oldLine = ctx.line
            val oldCol = ctx.col
            updatePos(ctx)

            if (postprocess(ctx)) {
                pc + 1
            } else {
                ctx.offset = oldOffset
                ctx.line = oldLine
                ctx.col = oldCol
                ctx.fail()
            }
        } else {
            ctx.expectedFailWithReason(expected, reason, numCodePoints)
        }
    }

    private def updatePos(ctx: Context): Unit = {
        ctx.offset += strsz
        ctx.line += numNewlines
        val startCol = if (numNewlines > 0) 1 else ctx.col // newline resets col
        val afterPre = startCol + charsPreFirstTab
        ctx.col = if (numTabs > 0) {
            val afterFirst = ((afterPre + 3) & -4) | 1 // first tab (col-dependent)
            val afterTabs = afterFirst + (numTabs - 1) * 4 // subsequent tabs always +4
            afterTabs + charsAfterLastTab
        } else {
            afterPre
        }
    }
}

private object Specific {
    private [Specific] def computePosInfo(s: String): (Int, Int, Int, Int) = {
        val lastNL = s.lastIndexOf('\n')
        val lineInc = s.count(_ == '\n')
        val suffix = if (lastNL < 0) s else s.substring(lastNL + 1)

        val firstTab = suffix.indexOf('\t')
        if (firstTab < 0) {
            (lineInc, suffix.length, 0, -1)
        } else {
            val lastTab = suffix.lastIndexOf('\t')
            val preFirstTab = firstTab // plain chars before first tab
            val numTabs = suffix.count(_ == '\t')
            val postLastTab = suffix.length - lastTab - 1 // plain chars after last tab
            (lineInc, preFirstTab, numTabs, postLastTab)
        }
    }
}

private [internal] final class SoftKeyword(protected val specific: String, letter: CharPredicate, protected val caseSensitive: Boolean,
                                           protected val expected: Iterable[ExpectItem], protected val reason: Option[String],
                                           expectedEnd: Iterable[ExpectDesc]) extends Specific {
    def this(specific: String, letter: CharPred, caseSensitive: Boolean, expected: LabelWithExplainConfig, expectedEnd: String) = {
        this(if (caseSensitive) specific else specific.toLowerCase,
             letter.asInternalPredicate,
             caseSensitive,
             expected.asExpectItems(specific), expected.asReason, Some(new ExpectDesc(expectedEnd)))
    }

    protected def postprocess(ctx: Context): Boolean = {
        if (letter.peek(ctx)) {
            ctx.expectedFail(expectedEnd, unexpectedWidth = 1) //This should only report a single token
            false
        }
        else {
            true
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = s"SoftKeyword($specific)"
    // $COVERAGE-ON$
}

private [internal] final class SoftOperator(protected val specific: String, letter: CharPredicate, ops: Trie[Unit],
                                            protected val expected: Iterable[ExpectItem], protected val reason: Option[String],
                                            expectedEnd: Iterable[ExpectDesc]) extends Specific {
    def this(specific: String, letter: CharPred, ops: Trie[Unit], expected: LabelWithExplainConfig, expectedEnd: String) = {
        this(specific, letter.asInternalPredicate, ops, expected.asExpectItems(specific), expected.asReason, Some(new ExpectDesc(expectedEnd)))
    }
    protected val caseSensitive = true
    private val ends = ops.suffixes(specific)

    // returns true if an end could be parsed from this point
    @tailrec private def checkEnds(ctx: Context, ends: Trie[Unit], off: Int, unexpectedWidth: Int): Int = {
        if (ends.nonEmpty && ctx.moreInput(off + 1)) {
            val endsOfNext = ends.suffixes(ctx.peekChar(off))
            checkEnds(ctx, endsOfNext, off + 1, if (endsOfNext.contains("")) off + 1 else unexpectedWidth)
        }
        else unexpectedWidth
    }

    protected def postprocess(ctx: Context): Boolean = {
        if (letter.peek(ctx)) {
            ctx.expectedFail(expectedEnd, unexpectedWidth = 1) //This should only report a single token
            false
        }
        else {
            val unexpectedWidth = checkEnds(ctx, ends, off = 0, unexpectedWidth = 0)
            if (unexpectedWidth != 0) {
                ctx.expectedFail(expectedEnd, unexpectedWidth)
                false
            }
            else {
                true
            }
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = s"SoftOperator($specific)"
    // $COVERAGE-ON$
}
