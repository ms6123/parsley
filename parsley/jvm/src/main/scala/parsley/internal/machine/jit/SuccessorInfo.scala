package parsley.internal.machine.jit

import parsley.internal.machine.instructions.Instr

case class SuccessorInfo(goodPaths: Set[Int], badPaths: Set[Int]) {
    def combined: Set[Int] = goodPaths ++ badPaths
}

object SuccessorInfo {
    def apply(instr: Instr, possibleHandlers: Iterable[List[Int]], pos: Int): SuccessorInfo = {
        val goodPaths = Set.newBuilder[Int]
        val badPaths = Set.newBuilder[Int]

        for (handlers <- possibleHandlers) {
            if (instr.fallThroughPath(handlers).isDefined) {
                goodPaths += (pos + 1)
            }
            goodPaths ++= instr.jumpPaths(handlers).map(_._2)
            badPaths ++= instr.failPath(handlers).map(_.head)
        }

        SuccessorInfo(goodPaths.result(), badPaths.result())
    }
}