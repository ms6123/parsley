package parsley.internal.machine.jit

import parsley.internal.machine.instructions.Instr
import parsley.internal.machine.{Context, ParseRunner}

object Optimizer {
  val useTco = true
  
  def optimize(instrs: Array[Instr]): ParseRunner = Context.interpreterRunner(instrs)
}
