package parsley.internal.machine.jit

import parsley.internal.machine.Context
import parsley.internal.machine.instructions.Instr

object Optimizer {
  private case class BasicBlock(start: Int, end: Int, instrs: Array[Instr]) extends Instr {
    override def apply(ctx: Context): Unit = {
      val startPc = ctx.pc
      for (instr <- instrs) {
        ctx.pc = startPc
        ctx.incs = 0
        instr(ctx)
        if (ctx.incs != 1) {
          require(ctx.incs == 0, "Instruction cannot increment pc more than once")
          // Diverged, return control
          return
        }
      }
      ctx.pc = startPc + 1
    }

    override def relabel(labels: Int => Int): this.type = ???

    override def labels(pos: Int): Seq[Int] = ???
  }

  def optimize(instrs: Array[Instr]): Array[Instr] = {
    val blockStarts = (instrs.view.zipWithIndex.flatMap { case (instr, pos) => instr.labels(pos) }.toSet + 0).toSeq.sorted

    val basicBlocks = {
      val blockEnds = blockStarts.tail.map(_ - 1) :+ (instrs.length - 1)
      blockStarts.zip(blockEnds).map { case (start, end) => BasicBlock(start, end + 1, instrs.slice(start, end + 1)) }
    }

    val blockStartToIndex = basicBlocks.zipWithIndex.map { case (block, idx) => block.start -> idx }.toMap

    for (instr <- instrs) {
      instr.relabel(blockStartToIndex)
    }

    basicBlocks.view.map(it => if (it.instrs.length == 1) it.instrs.head else it).toArray
  }
}
