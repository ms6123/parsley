package parsley.internal.machine.jit

import parsley.internal.machine.instructions.{Instr, StackInfo}

class FunctionInfo(val instrInfos: Array[InstrInfo])

object FunctionInfo {
    def apply(instrs: Array[Instr], allPossibleStacks: Array[Option[StackInfo]]): FunctionInfo = {
        val instrInfos = instrs.view.zip(allPossibleStacks).zipWithIndex.map { case ((instr, possibleStack), pos) =>
            InstrInfo(instr, possibleStack, pos)
        }.toArray
        new FunctionInfo(instrInfos)
    }
}

sealed trait AfterAction

object AfterAction {
    case class PopOperands(n: Int) extends AfterAction
}

case class Successor(pc: Int, afterActions: Seq[AfterAction])

case class InstrInfo(isReachable: Boolean, fallThroughPath: Option[Successor], jumpPaths: Seq[Successor], badPath: Option[Successor]) {
    def goodPaths: Seq[Successor] = fallThroughPath.toSeq ++ jumpPaths
}

object InstrInfo {
    def apply(instr: Instr, possibleStack: Option[StackInfo], pos: Int): InstrInfo = {
        possibleStack match {
            case None => InstrInfo(false, Option.empty, Seq.empty, Option.empty)
            case Some(StackInfo(stacksz, handlers)) =>
                val fallThroughPath = instr.fallThroughPath(stacksz, handlers).map { _ =>
                    Successor(pos + 1, Seq.empty)
                }
                val jumpPaths = instr.jumpPaths(stacksz, handlers).map { case (target, _) =>
                    Successor(target, Seq.empty)
                }
                val badPath = instr.failPath(stacksz, handlers).map { stackAfter =>
                    val activeHandler = stackAfter.handlers.head
                    Successor(activeHandler.pc, popActions(stackAfter.stacksz, activeHandler.stacksz))
                }
                InstrInfo(true, fallThroughPath, jumpPaths, badPath)
        }
    }
    
    private def popActions(startStack: Int, endStack: Int): Seq[AfterAction] =
        if (endStack >= startStack) {
            Seq.empty
        } else {
            Seq(AfterAction.PopOperands(startStack - endStack))
        }
}