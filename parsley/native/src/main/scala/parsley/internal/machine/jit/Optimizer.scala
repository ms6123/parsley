package parsley.internal.machine.jit

import parsley.internal.machine.{Context, ParseRunner}
import parsley.internal.machine.instructions.Instr

object Optimizer {
  val useTco = true
  val allowInlining = true
  
  def optimize(instrs: Array[Instr]): ParseRunner = Context.interpreterRunner(instrs)
}
