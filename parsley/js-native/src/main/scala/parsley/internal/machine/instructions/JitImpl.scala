package parsley.internal.machine.instructions

import scala.annotation.StaticAnnotation

final class JitImpl(val noop: Boolean = false,
                    val consumeOperands: Int = 0,
                    val intReturnKind: JitImpl.IntKind.Value = JitImpl.IntKind.Pc,
                    val beforeActions: Array[JitImpl.Action.Value] = Array(),
                    val afterActions: Array[JitImpl.Action.Value] = Array(),
                    val constants: Array[String] = Array(),
                    val params: Array[JitImpl.Param.Value] = Array(),
                    val updateCheckOffsets: Array[Int] = Array()
                   ) extends StaticAnnotation

object JitImpl {
    object Action extends Enumeration {
        val PushTrue, Swap, DupX1, Dup = Value
    }

    object Param extends Enumeration {
        val Pc, HandlerCheck = Value
    }

    object IntKind extends Enumeration {
        val Pc, Char, CodePoint = Value
    }
}
