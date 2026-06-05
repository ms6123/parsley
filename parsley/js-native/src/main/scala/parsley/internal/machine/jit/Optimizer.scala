package parsley.internal.machine.jit

import parsley.internal.machine.ParseRunner

object Optimizer {
  val isEnabled = false
  val useTco = true
  val allowInlining = true
  
  def optimize(params: Any*): ParseRunner = ???
}
