package parsley.internal.machine.jit

import parsley.internal.machine.instructions.Instr

case class SuccessorInfo(isReachable: Boolean, isHandler: Boolean, goodPaths: Set[Int], badPath: Option[Int])

object SuccessorInfo {
    def apply(instr: Instr, possibleHandlers: Iterable[List[Int]], pos: Int): SuccessorInfo = {
        require(possibleHandlers.sizeIs <= 1)
        if (possibleHandlers.isEmpty) {
            return SuccessorInfo(false, false, Set.empty, Option.empty)
        }
        val handlers = possibleHandlers.head

        val goodPaths = Set.newBuilder[Int]
        if (instr.fallThroughPath(handlers).isDefined) {
            goodPaths += (pos + 1)
        }
        goodPaths ++= instr.jumpPaths(handlers).map(_._2)

        val badPath = instr.failPath(handlers).map(_.head)

        SuccessorInfo(true, handlers.head == pos, goodPaths.result(), badPath)
    }
}