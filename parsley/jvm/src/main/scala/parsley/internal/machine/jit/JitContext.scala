package parsley.internal.machine.jit

import java.lang.invoke.MethodHandle

import parsley.XAssert.assert
import parsley.errors.ErrorBuilder

import parsley.internal.machine.Context
import parsley.internal.machine.errors.DefuncHints
import parsley.internal.machine.stacks.HandlerStack
import parsley.internal.machine.stacks.Stack.StackExt

import parsley.{Failure, Success}

private[jit] class JitContext(private val startMethod: MethodHandle,
                              input: String,
                              numRegs: Int,
                              sourceFile: Option[String]) extends Context(input, numRegs, sourceFile) {
    override private[machine] type HandlerStackT = JitHandlerStack

    private var calls = 0

    override private[parsley] def run[Err: ErrorBuilder, A]() = {
        //noinspection ScalaUnusedExpression
        startMethod.invokeExact(this): Unit
        if (good) {
            assert(stack.size == 1, s"stack must end a parse with exactly one item, it has ${stack.size}")
            assert(calls == 0, "there must be no more calls to unwind on end of parser")
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

    override private[machine] def call(at: Int): Unit = calls += 1

    override private[machine] def ret(): Unit = {
        assert(calls != 0, "cannot return when no calls are made")
        calls -= 1
    }

    override protected def failImpl(): Unit = {
        if (handlers.isEmpty) {
            running = false
            pc = -1
        } else {
            val handler = handlers
            if (calls == handler.calls) {
                // Local handler
                pc = handler.pc
            } else {
                // Handler in parent method
                pc = -1
            }
            calls = handler.calls
            val diffstack = stack.usize - handler.stacksz
            if (diffstack > 0) stack.drop(diffstack)
        }
    }

    override private[machine] def pushHandler(label: Int): Unit = {
        handlers = new JitHandlerStack(calls, label, stack.usize, offset, hints, hintsValidOffset, handlers)
    }

    // $COVERAGE-OFF$
    override private[machine] def pretty: String = {
        s"""[
           |  stack     = [${stack.mkString(", ")}]
           |  input     = ${input.drop(offset)}
           |  pos       = ($line, $col)
           |  status    = $status
           |  pc        = $pc
           |  rets      = $calls
           |  handlers  = ${handlers.mkString(", ")}
           |  recstates = ${states.mkString(", ")}
           |  registers = ${regs.zipWithIndex.map { case (r, i) => s"r$i = $r" }.toList.mkString("\n              ")}
           |  errors    = ${errs.mkString(", ")}
           |]""".stripMargin
    }
    // $COVERAGE-ON$
}

private class JitHandlerStack(val calls: Int,
                              pc: Int,
                              stacksz: Int,
                              check: Int,
                              hints: DefuncHints,
                              hintOffset: Int,
                              val tail: JitHandlerStack
                             ) extends HandlerStack[JitHandlerStack](pc, stacksz, check, hints, hintOffset)
