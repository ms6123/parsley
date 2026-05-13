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
import parsley.internal.machine.{Context, InterpreterContext}
import parsley.internal.machine.XAssert.*
import parsley.internal.machine.instructions.{Instr, JitImpl, SpecializedInstr}
import parsley.internal.machine.instructions.token.Specific.computePosUpdate

private [token] abstract class Specific extends Instr with SpecializedInstr {
    protected val specific: String
    val letter: CharPredicate
    val caseSensitive: Boolean
    protected val expected: Iterable[ExpectItem]
    protected val reason: Option[String]
    protected val expectedEnd: Iterable[ExpectDesc]
    private [this] final val strsz = specific.length
    private [this] final val numCodePoints = specific.codePointCount(0, strsz)
    val posUpdate: Context => Unit = computePosUpdate(specific)

    protected def postprocessNonLetter(ctx: Context): Boolean

    final override def apply(ctx: InterpreterContext, pc: Int): Int = {
        ensureRegularInstruction(ctx)
        if (ctx.input.regionMatches(!caseSensitive, ctx.offset, specific, 0, strsz)) {
            val oldOffset = ctx.offset
            val oldLine = ctx.line
            val oldCol = ctx.col
            posUpdate(ctx)

            val nextIsLetter = letter.peek(ctx)
            if (!nextIsLetter && postprocessNonLetter(ctx)) {
                pc + 1
            } else {
                val res = if (nextIsLetter) {
                    ctx.expectedFail(expectedEnd, unexpectedWidth = 1) //This should only report a single token
                } else {
                    ctx.fail()
                }

                ctx.offset = oldOffset
                ctx.line = oldLine
                ctx.col = oldCol

                res
            }
        } else {
            ctx.expectedFailWithReason(expected, reason, numCodePoints)
        }
    }

    @JitImpl(constants = Array("posUpdate", "caseSensitive", "letter"))
    def apply(posUpdate: Context => Unit, caseSensitive: Boolean, letter: CharPredicate, ctx: Context): Boolean = {
        if (ctx.input.regionMatches(!caseSensitive, ctx.offset, specific, 0, strsz)) {
            val oldOffset = ctx.offset
            val oldLine = ctx.line
            val oldCol = ctx.col
            posUpdate(ctx)

            if (!letter.peek(ctx) && postprocessNonLetter(ctx)) {
                true
            } else {
                ctx.offset = oldOffset
                ctx.line = oldLine
                ctx.col = oldCol
                false
            }
        } else {
            false
        }
    }
}

private object Specific {
    private [Specific] def computePosUpdate(s: String): Context => Unit = {
        val strsz = s.length
        val lastNL = s.lastIndexOf('\n')
        val lineInc = s.count(_ == '\n')
        val suffix = if (lastNL < 0) s else s.substring(lastNL + 1)

        val firstTab = suffix.indexOf('\t')
        if (firstTab < 0) {
            if (lineInc == 0) {
                ctx => {
                    ctx.offset += strsz
                    ctx.col += strsz
                }
            } else {
                ctx => {
                    ctx.offset += strsz
                    ctx.line += lineInc
                    ctx.col = 1
                }
            }
        } else {
            val lastTab = suffix.lastIndexOf('\t')
            val preFirstTab = firstTab // plain chars before first tab
            val numTabs = suffix.count(_ == '\t')
            val postLastTab = suffix.length - lastTab - 1 // plain chars after last tab

            ctx => {
                ctx.offset += strsz
                ctx.line += lineInc
                val startCol = if (lineInc > 0) 1 else ctx.col // newline resets col
                val afterPre = startCol + preFirstTab
                val afterFirst = ((afterPre + 3) & -4) | 1 // first tab (col-dependent)
                val afterTabs = afterFirst + (numTabs - 1) * 4 // subsequent tabs always +4
                ctx.col = afterTabs + postLastTab
            }
        }
    }
}

private [internal] final class SoftKeyword(protected val specific: String, val letter: CharPredicate, val caseSensitive: Boolean,
                                           protected val expected: Iterable[ExpectItem], protected val reason: Option[String],
                                           protected val expectedEnd: Iterable[ExpectDesc]) extends Specific {
    def this(specific: String, letter: CharPred, caseSensitive: Boolean, expected: LabelWithExplainConfig, expectedEnd: String) = {
        this(if (caseSensitive) specific else specific.toLowerCase,
             letter.asInternalPredicate,
             caseSensitive,
             expected.asExpectItems(specific), expected.asReason, Some(new ExpectDesc(expectedEnd)))
    }

    protected def postprocessNonLetter(ctx: Context): Boolean = true

    // $COVERAGE-OFF$
    override def toString: String = s"SoftKeyword($specific)"
    // $COVERAGE-ON$
}

private [internal] final class SoftOperator(protected val specific: String, val letter: CharPredicate, ops: Trie[Unit],
                                            protected val expected: Iterable[ExpectItem], protected val reason: Option[String],
                                            protected val expectedEnd: Iterable[ExpectDesc]) extends Specific {
    def this(specific: String, letter: CharPred, ops: Trie[Unit], expected: LabelWithExplainConfig, expectedEnd: String) = {
        this(specific, letter.asInternalPredicate, ops, expected.asExpectItems(specific), expected.asReason, Some(new ExpectDesc(expectedEnd)))
    }
    val caseSensitive = true
    private val ends = ops.suffixes(specific)

    // returns true if an end could be parsed from this point
    @tailrec private def checkEnds(ctx: Context, ends: Trie[Unit], off: Int, unexpectedWidth: Int): Int = {
        if (ends.nonEmpty && ctx.moreInput(off + 1)) {
            val endsOfNext = ends.suffixes(ctx.peekChar(off))
            checkEnds(ctx, endsOfNext, off + 1, if (endsOfNext.contains("")) off + 1 else unexpectedWidth)
        }
        else unexpectedWidth
    }

    protected def postprocessNonLetter(ctx: Context): Boolean = {
        val unexpectedWidth = checkEnds(ctx, ends, off = 0, unexpectedWidth = 0)
        if (unexpectedWidth != 0) {
            ctx.expectedFail(expectedEnd, unexpectedWidth)
            false
        }
        else {
            true
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = s"SoftOperator($specific)"
    // $COVERAGE-ON$
}
