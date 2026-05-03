package parsley.internal.machine.jit

import java.lang.invoke.MethodHandle

import parsley.XAssert.assert
import parsley.errors.ErrorBuilder

import parsley.internal.errors.ExpectItem
import parsley.internal.machine.Context
import parsley.internal.machine.errors.DefuncError
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
        startMethod.invokeExact(this): Boolean
        if (good) {
            assert(stack.size == 1, s"stack must end a parse with exactly one item, it has ${stack.size}")
            assert(handlers.isEmpty, "there must be no more handlers on end of parse")
            assert(states.isEmpty, "there must be no residual states left at end of parse")
            Success(stack.peek[A])
        }
        else {
            assert(handlers.isEmpty, "there must be no more handlers on end of parse")
            assert(states.isEmpty, "there must be no residual states left at end of parse")
            Failure(null.asInstanceOf[Err])
        }
    }

    override private[machine] def call(at: Int): Int = ???

    override private[machine] def ret(): Int = ???

    override protected def failImpl(): Int = {
        if (!handlers.isEmpty) {
            val handler = handlers
            val diffstack = stack.usize - handler.stacksz
            if (diffstack > 0) stack.drop(diffstack)
        }
        -1
    }

    override private[machine] def pushHandler(label: Int): Unit = {
        handlers = new JitHandlerStack(stack.usize, offset, handlers)
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
           |  handlers  = ${handlers.mkString(", ")}
           |  recstates = ${states.mkString(", ")}
           |  registers = ${regs.zipWithIndex.map { case (r, i) => s"r$i = $r" }.toList.mkString("\n              ")}
           |]""".stripMargin
    }
    // $COVERAGE-ON$

    // Error handling
    override private[machine] def mergeHints(): Unit = ()

    override private[machine] def popHints(): Unit = ()

    override private[machine] def replaceHint(labels: Iterable[String]): Unit = ()

    override private[machine] def restoreHints(): Unit = ()

    override private[machine] def addErrorToHintsAndPop(): Unit = ()

    override private[machine] def addHints(expecteds: Set[ExpectItem], unexpectedWidth: Int): Unit = ()

    override private[machine] def clearHints(): Unit = ()

    override private[machine] def pushError(err: => DefuncError): Unit = ()

    override private[machine] def popError(): Unit = ()

    override private[machine] def relabelError(labels: Iterable[String]): Unit = ()

    override private[machine] def hideError(): Unit = ()

    override private[machine] def mergeErrors(): Unit = ()

    override private[machine] def applyReason(reason: String): Unit = ()

    override private[machine] def amendError(partial: Boolean): Unit = ()

    override private[machine] def entrenchError(): Unit = ()

    override private[machine] def dislodgeError(n: Int): Unit = ()

    override private[machine] def markErrorAsLexical(): Unit = ()
}

private final class JitHandlerStack(val stacksz: Int,
                                    var check: Int,
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