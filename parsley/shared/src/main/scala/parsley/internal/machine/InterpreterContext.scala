package parsley.internal.machine

import parsley.errors.ErrorBuilder
import parsley.XAssert.{assert, assume}

import parsley.internal.errors.ExpectItem
import parsley.internal.machine.errors.{DefuncError, DefuncHints, EmptyError, EmptyHints, ExpectedError}
import parsley.internal.machine.instructions.Instr
import parsley.internal.machine.stacks.{ArrayStack, CallStack, HandlerStack, Stack}
import parsley.internal.machine.stacks.Stack.StackExt

import parsley.{Failure, Result, Success}

private[machine] class InterpreterContext(private[this] val startInstrs: Array[Instr],
                                          input: String,
                                          numRegs: Int,
                                          sourceFile: Option[String]) extends Context(input, numRegs, sourceFile) {
    /** Current operational status of the machine */
    override private[machine] var good: Boolean = true
    private[machine] var running: Boolean = true
    /** This is the operand stack, where results go to live */
    private[machine] val stack: ArrayStack[Any] = new ArrayStack()
    private [machine] var handlers: InterpreterHandlerStack = Stack.empty
    /** Current offset into program instruction buffer */
    private[machine] var pc: Int = 0

    private [machine] var instrs: Array[Instr] = _
    /** Call stack consisting of Frames that track the return position and the old instructions */
    private[machine] var calls: CallStack = Stack.empty
    
    // NEW ERROR MECHANISMS
    private[machine] var hints: DefuncHints = EmptyHints
    protected var hintsValidOffset = 0
    private[machine] val errs: ArrayStack[DefuncError] = new ArrayStack()
    /** Amount of indentation to apply to debug combinators output */
    private[machine] var debuglvl: Int = 0

    override private[machine] def restoreHints(): Unit = {
        val hintFrame = this.handlers
        this.hintsValidOffset = hintFrame.hintOffset
        this.hints = hintFrame.hints
    }

    /* Error Debugging Info */
    private[machine] def inFlightHints: DefuncHints = hints

    private[machine] def inFlightError: DefuncError = errs.peek

    private[machine] def currentHintsValidOffset: Int = hintsValidOffset

    /* ERROR RELABELLING BEGIN */
    override private[machine] def mergeHints(): Unit = {
        val hintFrame = this.handlers
        if (hintFrame.hintOffset == offset) this.hints = hintFrame.hints.merge(this.hints)
    }

    override private[machine] def replaceHint(labels: Iterable[String]): Unit = hints = hints.rename(labels)

    override private[machine] def popHints(): Unit = hints = hints.pop
    /* ERROR RELABELLING END */

    private def invalidateHints(): Unit = {
        if (hintsValidOffset < offset) {
            hints = EmptyHints
            hintsValidOffset = offset
        }
    }

    private def addErrorToHints(err: DefuncError): Unit = {
        assume(!(!err.isExpectedEmpty) || err.isTrivialError, "not having an empty expected implies you are a trivial error")
        if ( /*err.isTrivialError && */ !err.isExpectedEmpty && err.presentationOffset == offset) { // scalastyle:ignore disallow.space.after.token
            // If our new hints have taken place further in the input stream, then they must invalidate the old ones
            invalidateHints()
            hints = hints.addError(err)
        }
    }

    override private[machine] def addErrorToHintsAndPop(): Unit = {
        this.addErrorToHints(errs.pop())
    }

    override private [machine] def addHints(expecteds: Set[ExpectItem], unexpectedWidth: Int): Unit = {
        assume(expecteds.nonEmpty, "hints must always be non-empty")
        invalidateHints()
        hints = hints.addError(new ExpectedError(this.offset, this.line, this.col, expecteds, unexpectedWidth)) // TODO: this can be optimised further
    }
    
    override private [machine] def clearHints(): Unit = hints = EmptyHints

    override private[machine] def handlerCheck = handlers.check

    override private[machine] def handlerCheck_=(v: Int): Unit = handlers.check = v

    override private [machine] def pushError(err: =>DefuncError): Unit = errs.push(this.useHints(err))

    override private[machine] def popError(): Unit = errs.pop_()
    
    override private[machine] def relabelError(labels: Iterable[String]): Unit = {
        errs.exchange(useHints {
            // only use the label if the error message is generated at the same offset
            // as the check stack saved for the start of the `label` combinator.
            errs.peek.label(labels, handlers.check)
        })
    }

    override private[machine] def hideError(): Unit = {
        errs.exchange(new EmptyError(offset, line, col, unexpectedWidth = 0))
    }

    override private[machine] def mergeErrors(): Unit = {
        val err2 = errs.pop[DefuncError]()
        errs.exchange(errs.peek.merge(err2))
    }

    override private[machine] def applyReason(reason: String): Unit = {
        errs.exchange(errs.peek.withReason(reason, handlers.check))
    }

    override private[machine] def amendError(partial: Boolean): Unit = {
        errs.exchange(errs.peek.amend(partial, states.offset, states.line, states.col))
    }

    override private[machine] def entrenchError(): Unit = {
        errs.exchange(errs.peek.entrench)
    }

    override private[machine] def dislodgeError(n: Int): Unit = {
        errs.exchange(errs.peek.dislodge(n))
    }

    override private[machine] def markErrorAsLexical(): Unit = {
        errs.exchange(errs.peek.markAsLexical(handlers.check))
    }

    private[machine] def useHints(err: DefuncError): DefuncError = {
        if (hintsValidOffset == err.presentationOffset) err.withHints(hints)
        else {
            hintsValidOffset = err.presentationOffset
            hints = EmptyHints
            err
        }
    }

    def run[Err: ErrorBuilder, A](): Result[Err, A] = {
        instrs = startInstrs
        while (running) {
            pc = instrs(pc)(this, pc)
        }
        if (good) {
            assert(stack.size == 1, s"stack must end a parse with exactly one item, it has ${stack.size}")
            assert(calls.isEmpty, "there must be no more calls to unwind on end of parser")
            assert(handlers.isEmpty, "there must be no more handlers on end of parse")
            assert(states.isEmpty, "there must be no residual states left at end of parse")
            assert(errs.isEmpty, "there should be no parse errors remaining at end of parse")
            Success(stack.peek[A])
        }
        else {
            assert(!errs.isEmpty && errs.size == 1, "there should be exactly 1 parse error remaining at end of parse")
            assert(handlers.isEmpty, "there must be no more handlers on end of parse")
            assert(states.isEmpty, "there must be no residual states left at end of parse")
            Failure(errs.peek.asParseError.format(sourceFile))
        }
    }

    private [machine] def call(at: Int): Int = {
        calls = new CallStack(pc + 1, instrs, at, calls)
        at
    }

    private [machine] def ret(): Int = {
        assert(calls != null, "cannot return when no calls are made")
        instrs = calls.instrs
        val newPc = calls.ret
        calls = calls.tail
        newPc
    }

    override private[machine] def catchNoConsumed(check: Int)(handler: => Int): Int = {
        assert(!good, "catching can only be performed in a handler")
        if (offset != check) {
            popHandler()
            fail()
        }
        else {
            good = true
            handler
        }
    }

    override private [machine] def fail(error: => DefuncError): Int = {
        good = false
        this.pushError(error)
        this.fail()
    }

    override private [machine] def fail(): Int = {
        assert(!good, "fail() may only be called in a failing context, use `fail(err)` or set `good = false`")
        if (handlers.isEmpty) {
            running = false
            pc
        }
        else {
            val handler = handlers
            instrs = handler.instrs
            calls = handler.calls
            val diffstack = stack.usize - handler.stacksz
            if (diffstack > 0) stack.drop(diffstack)
            handler.pc
        }
    }

    private[machine] def push(x: Any) = {
        stack.push(x)
    }

    private[machine] def unsafePush(x: Any) = {
        stack.upush(x)
    }

    private[machine] def exchange(x: Any) = {
        stack.exchange(x)
    }

    override private[machine] def pushHandler(label: Int): Unit = {
        handlers = new InterpreterHandlerStack(calls, instrs, label, stack.usize, offset, hints, hintsValidOffset, handlers)
    }

    override private[machine] def popHandler(): Unit = {
        handlers = handlers.tail
    }

    override private[machine] def replaceHandler(label: Int): Unit = {
        handlers.pc = label
    }

    // $COVERAGE-OFF$
    override private[machine] def pretty: String = {
        s"""[
           |  stack     = [${stack.mkString(", ")}]
           |  instrs    = ${instrs.toList.mkString("; ")}
           |  input     = ${input.drop(offset)}
           |  pos       = ($line, $col)
           |  status    = $status
           |  pc        = $pc
           |  rets      = ${calls.mkString(", ")}
           |  handlers  = ${handlers.mkString(", ")}
           |  recstates = ${states.mkString(", ")}
           |  registers = ${regs.zipWithIndex.map { case (r, i) => s"r$i = $r" }.toList.mkString("\n              ")}
           |  errors    = ${errs.mkString(", ")}
           |]""".stripMargin
    }
    // $COVERAGE-ON$

    private[machine] def status: Status = {
        if (running) if (good) Good else Recover
        else if (good) Finished else Failed
    }
}

private class InterpreterHandlerStack(val calls: CallStack,
                                      val instrs: Array[Instr],
                                      var pc: Int,
                                      val stacksz: Int,
                                      var check: Int,
                                      val hints: DefuncHints,
                                      val hintOffset: Int,
                                      val tail: InterpreterHandlerStack
                                     ) extends HandlerStack

private [machine] object InterpreterHandlerStack extends Stack[InterpreterHandlerStack] {
    type ElemTy = (Int, Int)

    // $COVERAGE-OFF$
    implicit val inst: Stack[InterpreterHandlerStack] = this

    // TODO: needs to change
    override protected def show(x: ElemTy): String = {
        val (pc, stacksz) = x
        s"Handler:$pc(-${stacksz + 1})"
    }

    override protected def head(xs: InterpreterHandlerStack): ElemTy = (xs.pc, xs.stacksz)

    override protected def tail(xs: InterpreterHandlerStack): InterpreterHandlerStack = xs.tail
    // $COVERAGE-ON$
}