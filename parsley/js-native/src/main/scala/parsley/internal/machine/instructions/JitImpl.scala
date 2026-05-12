package parsley.internal.machine.instructions

import scala.annotation.StaticAnnotation

final class JitImpl(val noop: Boolean = false,
                    val consumeOperands: Int = 0,
                    val beforeActions: Array[JitImpl.Action.Value] = Array(),
                    val afterActions: Array[JitImpl.Action.Value] = Array(),
                    val params: Array[JitImpl.Param.Value] = Array()
                   ) extends StaticAnnotation

object JitImpl {
    object Action extends Enumeration {
        val PushTrue, Swap, DupX1, UpdateCheckOffset = Value
    }

    object Param extends Enumeration {
        val Pc, HandlerCheck = Value
    }
}
