package parsley.internal.machine.jit

import parsley.internal.machine.instructions.{Instr, StackInfo}

class FunctionInfo(val instrInfos: Array[Option[InstrInfo]])

sealed trait AfterAction

object AfterAction {
    case class PopOperands(n: Int) extends AfterAction
}

case class Successor(pc: Int, afterActions: Seq[AfterAction])

case class InstrInfo(stackInfo: StackInfo, fallThroughPath: Option[Successor], jumpPaths: Seq[Successor], badPath: Option[Successor]) {
    def goodPaths: Seq[Successor] = fallThroughPath.toSeq ++ jumpPaths

    def allPaths: Seq[Successor] = goodPaths ++ badPath.toSeq
}

object InstrInfo {
    def apply(instr: Instr, pos: Int, possibleStack: Option[StackInfo], canSucceed: Boolean = true, canFail: Boolean = true): Option[InstrInfo] =
        possibleStack match {
            case None =>
                None
            case Some(stackInfo@StackInfo(stacksz, handlers)) =>
                val fallThroughPath = instr.fallThroughPath(stacksz, handlers).filter(_ => canSucceed).map { _ =>
                    Successor(pos + 1, Seq.empty)
                }
                val jumpPaths = instr.jumpPaths(stacksz, handlers).filter(_ => canSucceed).map { case (target, _) =>
                    Successor(target, Seq.empty)
                }
                val badPath = instr.failPath(stacksz, handlers).filter(_ => canFail).map { stackAfter =>
                    val activeHandler = stackAfter.handlers.head
                    Successor(activeHandler.pc, popActions(stackAfter.stacksz, activeHandler.stacksz))
                }
                Some(InstrInfo(stackInfo, fallThroughPath, jumpPaths, badPath))
        }

    private def popActions(startStack: Int, endStack: Int): Seq[AfterAction] =
        if (endStack >= startStack) {
            Seq.empty
        } else {
            Seq(AfterAction.PopOperands(startStack - endStack))
        }
}