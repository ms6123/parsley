package parsley.internal.machine.jit.codegen

import java.lang.reflect.Method

import scala.collection.mutable

import parsley.internal.machine.Context
import parsley.internal.machine.instructions.{Instr, WhiteSpaceLike}
import parsley.internal.machine.jit.JitContext

private [codegen] object Methods {
    object Context {
        val IS_GOOD: Method = classOf[JitContext].getMethod("good")
        val GET_OFFSET: Method = classOf[JitContext].getMethod("offset")
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
}
