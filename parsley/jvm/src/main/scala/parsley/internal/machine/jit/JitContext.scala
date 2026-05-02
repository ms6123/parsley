package parsley.internal.machine.jit

import java.lang.invoke.MethodHandle

import parsley.XAssert.assert
import parsley.errors.ErrorBuilder

import parsley.internal.machine.Context
import parsley.internal.machine.errors.DefuncHints
import parsley.internal.machine.stacks.{HandlerStack, Stack}
import parsley.internal.machine.stacks.Stack.StackExt

import parsley.{Failure, Result, Success}

private[jit] final class JitContext(private val startMethod: MethodHandle,
                              input: String,
                              numRegs: Int,
                              sourceFile: Option[String]) extends Context(input, numRegs, sourceFile) {
    private[machine] var handlers: JitHandlerStack = Stack.empty

    def run[Err: ErrorBuilder, A](): Result[Err, A] = {
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

    override private[machine] def popHandler(): Unit = {
        handlers = handlers.tail
    }

    override private[machine] def replaceHandler(label: Int): Unit = ()

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

private final class JitHandlerStack(val stacksz: Int,
                                    var check: Int,
                                    val hints: DefuncHints,
                                    val hintOffset: Int,
                                    val tail: JitHandlerStack,
                                   ) extends HandlerStack

private [machine] object JitHandlerStack extends Stack[JitHandlerStack] {
    type ElemTy = Int

    // $COVERAGE-OFF$
    implicit val inst: Stack[JitHandlerStack] = this

    // TODO: needs to change
    override protected def show(x: Int): String = {
        s"Handler:(-${x + 1})"
    }

    override protected def head(xs: JitHandlerStack): ElemTy = xs.stacksz

    override protected def tail(xs: JitHandlerStack): JitHandlerStack = xs.tail
    // $COVERAGE-ON$
}