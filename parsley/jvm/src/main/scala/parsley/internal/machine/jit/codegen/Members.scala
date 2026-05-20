package parsley.internal.machine.jit.codegen

import java.lang.reflect.{Field, Method}

import scala.collection.mutable
import scala.runtime.BoxesRunTime

import parsley.internal.machine.Context
import parsley.internal.machine.instructions.{Instr, WhiteSpaceLike}
import parsley.internal.machine.jit.{Continuation, ContinuationResult, JitContext}

private [codegen] object Members {
    object Context {
        val IS_GOOD: Method = classOf[JitContext].getMethod("good")
        val GET_OFFSET: Method = classOf[JitContext].getMethod("offset")
        val GET_RESULT_HOLDER: Method = classOf[JitContext].getMethod("resultHolder")
    }

    object Instr {
        val APPLY: Method = classOf[Instr].getMethod("apply", classOf[Context], classOf[Int])
    }

    object Either {
        val LEFT_VALUE: Method = classOf[Left[?, ?]].getMethod("value")
        val RIGHT_VALUE: Method = classOf[Right[?, ?]].getMethod("value")
    }

    object Builder {
        val RESULT: Method = classOf[mutable.Builder[?, ?]].getMethod("result")
        val ADD_ONE: Method = classOf[mutable.Builder[?, ?]].getMethod("$plus$eq", classOf[Any])
    }

    object Functions {
        val APPLY0: Method = classOf[Function0[?]].getMethod("apply")
    }

    object WhiteSpaceLikeImpl {
        val APPLY: Method = classOf[WhiteSpaceLike.Impl].getMethod("apply", classOf[Context])
    }

    object Continuation {
        val RUN: Method = classOf[Continuation].getMethod("run", classOf[Continuation], classOf[JitContext])
        val RESULT: Field = classOf[Continuation].getField("result")
        val NEXT: Field = classOf[Continuation].getField("next")
    }

    object Boxing {
        val BOX_TO_BOOLEAN: Method = classOf[BoxesRunTime].getMethod("boxToBoolean", classOf[Boolean])
        val UNBOX_TO_BOOLEAN: Method = classOf[BoxesRunTime].getMethod("unboxToBoolean", classOf[AnyRef])
    }

    object Boolean {
        val TRUE: Field = classOf[java.lang.Boolean].getField("TRUE")
        val FALSE: Field = classOf[java.lang.Boolean].getField("FALSE")
    }
}
