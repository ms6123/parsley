package parsley.internal

import parsley.internal.machine.ParseRunner

sealed trait UseJit

object UseJit {
    case class Yes(fallback: () => ParseRunner) extends UseJit
    case object No extends UseJit
}
