package parsley.internal.machine

import parsley.errors.ErrorBuilder
import parsley.XAssert.assert

import parsley.internal.machine.errors.DefuncHints
import parsley.internal.machine.instructions.Instr
import parsley.internal.machine.stacks.{CallStack, HandlerStack}
import parsley.internal.machine.stacks.Stack
import parsley.internal.machine.stacks.Stack.StackExt

import parsley.{Failure, Success}

private[machine] class InterpreterContext(private[this] val startInstrs: Array[Instr],
                                          input: String,
                                          numRegs: Int,
                                          sourceFile: Option[String]) extends Context(input, numRegs, sourceFile) {
    override private[machine] type HandlerStackT = InterpreterHandlerStack

    private [machine] var instrs: Array[Instr] = _
    /** Call stack consisting of Frames that track the return position and the old instructions */
    private[machine] var calls: CallStack = Stack.empty

    override private[parsley] def run[Err: ErrorBuilder, A]() = {
        instrs = startInstrs
        while (running) {
            instrs(pc)(this)
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
            assert(!errs.isEmpty && errs.tail.isEmpty, "there should be exactly 1 parse error remaining at end of parse")
            assert(handlers.isEmpty, "there must be no more handlers on end of parse")
            assert(states.isEmpty, "there must be no residual states left at end of parse")
            Failure(errs.error.asParseError.format(sourceFile))
        }
    }

    override private[machine] def call(at: Int): Unit = {
        calls = new CallStack(pc + 1, instrs, at, calls)
        pc = at
    }

    override private[machine] def ret(): Unit = {
        assert(calls != null, "cannot return when no calls are made")
        instrs = calls.instrs
        pc = calls.ret
        calls = calls.tail
    }

    override def failImpl(): Unit = {
        if (handlers.isEmpty) {
            running = false
        }
        else {
            val handler = handlers
            instrs = handler.instrs
            calls = handler.calls
            pc = handler.pc
            val diffstack = stack.usize - handler.stacksz
            if (diffstack > 0) stack.drop(diffstack)
        }
    }

    override private[machine] def pushHandler(label: Int): Unit = {
        handlers = new InterpreterHandlerStack(calls, instrs, label, stack.usize, offset, hints, hintsValidOffset, handlers)
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
}

private class InterpreterHandlerStack(val calls: CallStack,
                                      val instrs: Array[Instr],
                                      pc: Int,
                                      stacksz: Int,
                                      check: Int,
                                      hints: DefuncHints,
                                      hintOffset: Int,
                                      val tail: InterpreterHandlerStack
                                     ) extends HandlerStack[InterpreterHandlerStack](pc, stacksz, check, hints, hintOffset)
