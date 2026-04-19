package parsley.internal.machine.jit

import parsley.internal.machine.instructions.Instr

object Optimizer {
  val useTco = true
  
  def optimize(instrs: Array[Instr]): Array[Instr] = {
    instrs
  }
}
