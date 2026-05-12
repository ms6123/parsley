/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine

import scala.annotation.tailrec

//import parsley.{Failure, Result, Success} // not sure why this fails scalacheck, but I guess we'll leave it until I can submit a bug report
import parsley.Failure
import parsley.Result
import parsley.Success
import parsley.XAssert.*
import parsley.errors.ErrorBuilder

import parsley.internal.errors.{CaretWidth, ExpectItem, LineBuilder, UnexpectDesc}
import parsley.internal.machine.errors.{ClassicFancyError, DefuncError, DefuncHints, EmptyHints,
                                        ErrorItemBuilder, ExpectedError, ExpectedErrorWithReason, UnexpectedError}

import instructions.Instr
import stacks.{ArrayStack, CallStack, ErrorStack, HandlerStack, Stack, StateStack}, Stack.StackExt

private [parsley] abstract class Context(private[machine] val input: String,
                                        numRegs: Int,
                                        private val sourceFile: Option[String]) {
    /** Current offset into the input */
    private [machine] var offset: Int = 0
    /** The length of the input, stored for whatever reason */
    private [machine] val inputsz: Int = input.length
    /** State stack consisting of offsets and positions that can be rolled back */
    private [machine] var states: StateStack = Stack.empty
    private [machine] def good: Boolean = ???
    private [machine] def good_=(v: Boolean): Unit
    /** Current line number */
    private [machine] var line: Int = 1
    /** Current column number */
    private [machine] var col: Int = 1
    /** State held by the registers, AnyRef to allow for `null` */
    private [machine] var regs: Array[AnyRef] = new Array[AnyRef](numRegs)

    /* ERROR RELABELLING BEGIN */
    private[machine] def mergeHints(): Unit

    private[machine] def replaceHint(labels: Iterable[String]): Unit

    private[machine] def popHints(): Unit
    /* ERROR RELABELLING END */

    private [machine] def restoreHints(): Unit

    private [machine] def addErrorToHintsAndPop(): Unit

    private [machine] def addHints(expecteds: Set[ExpectItem], unexpectedWidth: Int): Unit

    private [machine] def clearHints(): Unit

    private [machine] def pretty: String

    private [machine] def catchNoConsumed(check: Int)(handler: =>Int): Int

    private [machine] def popError(): Unit

    private [machine] def relabelError(labels: Iterable[String]): Unit

    private [machine] def hideError(): Unit

    private [machine] def mergeErrors(): Unit

    private [machine] def applyReason(reason: String): Unit

    private [machine] def amendError(partial: Boolean): Unit

    private [machine] def entrenchError(): Unit

    private [machine] def dislodgeError(n: Int): Unit

    private [machine] def markErrorAsLexical(): Unit

    private [machine] def failWithMessage(caretWidth: CaretWidth, msgs: String*): Int
    
    private [machine] def unexpectedFail(expected: Iterable[ExpectItem], unexpected: UnexpectDesc): Int
    
    private [machine] def expectedFail(expected: Iterable[ExpectItem], unexpectedWidth: Int): Int
    
    private [machine] def expectedFailWithReason(expected: Iterable[ExpectItem], reason: String, unexpectedWidth: Int): Int
    
    private [machine] def expectedFailWithReason(expected: Iterable[ExpectItem], reason: Option[String], unexpectedWidth: Int): Int

    private [machine] def fail(): Int
    
    private [machine] def peekChar: Char = input.charAt(offset)
    private [machine] def peekChar(lookAhead: Int): Char = input.charAt(offset + lookAhead)
    private [machine] def moreInput: Boolean = offset < inputsz
    private [machine] def moreInput(n: Int): Boolean = offset + (n - 1) < inputsz
    private [machine] def updatePos(c: Char) = c match {
        case '\n' => line += 1; col = 1
        case '\t' => col = ((col + 3) & -4) | 1//((col - 1) | 3) + 2 // scalastyle:ignore magic.number
        case _    => col += 1
    }
    private [machine] def consumeChar(): Char = {
        val c = peekChar
        updatePos(c)
        offset += 1
        c
    }
    private [machine] def fastConsumeSupplementaryChar(): Unit = {
        assert(this.peekChar.isHighSurrogate, "must have a high surrogate to consume supplementary")
        // not going to be a tab or newline
        offset += 2
        col += 1
    }
    private [machine] def fastUncheckedConsumeChars(n: Int): Unit = {
        offset += n
        col += n
    }
    private [machine] def refreshState(): Unit = {
        val state = states
        state.offset = offset
        state.line = line
        state.col = col
    }
    private [machine] def saveState(): Unit = states = new StateStack(offset, line, col, states)
    private [machine] def restoreState(): Unit = {
        val state = states
        states = states.tail
        offset = state.offset
        line = state.line
        col = state.col
    }
    private [machine] def writeReg(reg: Int, x: Any): Unit = {
        regs(reg) = x.asInstanceOf[AnyRef]
    }

    protected implicit val lineBuilder: LineBuilder = new LineBuilder {
        def nearestNewlineBefore(off: Int): Option[Int] = {
            if (off < 0) None
            else Some {
                val idx = Context.this.input.lastIndexOf('\n', off-1)
                if (idx == -1) 0 else idx + 1
            }
        }
        def nearestNewlineAfter(off: Int): Option[Int] = {
            if (off > Context.this.inputsz) None
            else Some {
                val idx = Context.this.input.indexOf('\n', off)
                if (idx == -1) Context.this.inputsz else idx
            }
        }
        def segmentBetween(start: Int, end: Int): String = {
            Context.this.input.substring(start, end)
        }
    }

    private [machine] implicit val errorItemBuilder: ErrorItemBuilder = new ErrorItemBuilder {
        def inRange(offset: Int): Boolean = offset < Context.this.inputsz
        def codePointAt(offset: Int): Int = Context.this.input.codePointAt(offset)
        //def substring(offset: Int, size: Int): String = Context.this.input.substring(offset, Math.min(offset + size, Context.this.inputsz))
        def iterableFrom(offset: Int): IndexedSeq[Char] = Context.this.input.substring(offset)
    }
}

private [parsley] object Context {
    def interpreterRunner(instrs: Array[Instr]): ParseRunner = new ParseRunner {
        override def run[Err: ErrorBuilder, A](input: String, numRegs: Int, sourceFile: Option[String]): Result[Err, A] =
            new InterpreterContext(instrs, input, numRegs, sourceFile).run()

        override def dynCall(ctx: Context, pc: Int): Int =
            ctx match {
                case ctx: InterpreterContext =>
                    ctx.call(0)
                    ctx.instrs = instrs
                    0
            }
    }
}
