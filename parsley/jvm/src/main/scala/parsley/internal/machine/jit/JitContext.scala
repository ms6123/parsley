package parsley.internal.machine.jit

import java.lang.invoke.MethodHandle

import parsley.XAssert.assert
import parsley.errors.ErrorBuilder

import parsley.internal.errors.ExpectItem
import parsley.internal.machine.Context
import parsley.internal.machine.errors.DefuncError
import parsley.internal.machine.instructions.FailMarker
import parsley.internal.machine.stacks.{HandlerStack, Stack}
import parsley.internal.machine.stacks.Stack.StackExt

import parsley.{Failure, Result, Success}

private[jit] final class JitContext(private val startMethod: MethodHandle,
                                    input: String,
                                    numRegs: Int,
                                    sourceFile: Option[String]) extends Context(input, numRegs, sourceFile) {
    private[machine] var handlers: JitHandlerStack = Stack.empty

    def run[Err: ErrorBuilder, A](): Result[Err, A] = {
        val result = startMethod.invokeExact(this)
        result match {
            case FailMarker =>
                assert(handlers.isEmpty, "there must be no more handlers on end of parse")
                assert(states.isEmpty, "there must be no residual states left at end of parse")
                Failure(null.asInstanceOf[Err])
            case _ =>
                assert(handlers.isEmpty, "there must be no more handlers on end of parse")
                assert(states.isEmpty, "there must be no residual states left at end of parse")
                Success(result.asInstanceOf[A])
        }
    }

    override private[machine] def good_=(v: Boolean): Unit = ()

    override private[machine] def catchNoConsumed(check: Int)(handler: => Int): Int = {
        if (offset != check) {
            popHandler()
            fail()
        }
        else {
            handler
        }
    }

    override private [machine] def fail(error: =>DefuncError): Int = fail()

    override private [machine] def fail(): Int = -1

    override private[machine] def pushHandler(label: Int): Unit = {
        handlers = new JitHandlerStack(offset, handlers)
    }

    override private[machine] def popHandler(): Unit = {
        handlers = handlers.tail
    }

    override private[machine] def replaceHandler(label: Int): Unit = ()

    // $COVERAGE-OFF$
    override private[machine] def pretty: String = {
        s"""[
           |  input     = ${input.drop(offset)}
           |  pos       = ($line, $col)
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

private final class JitHandlerStack(var check: Int,
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

    override protected def head(xs: JitHandlerStack): ElemTy = xs.check

    override protected def tail(xs: JitHandlerStack): JitHandlerStack = xs.tail
    // $COVERAGE-ON$
}