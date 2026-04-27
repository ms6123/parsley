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

    override private[parsley] def run[Err: ErrorBuilder, A]() = {
        //noinspection ScalaUnusedExpression
        startMethod.invokeExact(this): Unit
        if (good) {
            assert(stack.size == 1, s"stack must end a parse with exactly one item, it has ${stack.size}")
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

    override private[machine] def call(at: Int): Unit = ???

    override private[machine] def ret(): Unit = ???

    override protected def failImpl(): Unit = {
        if (!handlers.isEmpty) {
            val handler = handlers
            val diffstack = stack.usize - handler.stacksz
            if (diffstack > 0) stack.drop(diffstack)
        }
    }

    override private[machine] def pushHandler(label: Int): Unit = {
        handlers = new JitHandlerStack(stack.usize, offset, hints, hintsValidOffset, handlers)
    }

    // $COVERAGE-OFF$
    override private[machine] def pretty: String = {
        s"""[
           |  stack     = [${stack.mkString(", ")}]
           |  input     = ${input.drop(offset)}
           |  pos       = ($line, $col)
           |  status    = $status
           |  pc        = $pc
           |  handlers  = ${handlers.mkString(", ")}
           |  recstates = ${states.mkString(", ")}
           |  registers = ${regs.zipWithIndex.map { case (r, i) => s"r$i = $r" }.toList.mkString("\n              ")}
           |  errors    = ${errs.mkString(", ")}
           |]""".stripMargin
    }
    // $COVERAGE-ON$
}

private class JitHandlerStack(stacksz: Int,
                              check: Int,
                              hints: DefuncHints,
                              hintOffset: Int,
                              val tail: JitHandlerStack
                             ) extends HandlerStack[JitHandlerStack](stacksz, check, hints, hintOffset) {
    override def pc: Int = -1

    override def pc_=(v: Int): Unit = ()
}
