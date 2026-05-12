package parsley.internal.machine.jit

import scala.collection.mutable

import parsley.internal.machine.instructions.{HandlerInfo, Instr, SpecializedInstr, StackInfo}
import parsley.internal.machine.instructions.JitImpl.Param

class FunctionInfo(val instrInfos: Array[Option[InstrInfo]], val handlerSlots: Map[Int, Int])

object FunctionInfo {
    def apply(instrs: Array[Instr], instrInfos: Array[Option[InstrInfo]]): FunctionInfo = {
        new FunctionInfo(instrInfos, allocateHandlerSlots(instrs, instrInfos))
    }

    private def allocateHandlerSlots(instrs: Array[Instr], instrInfos: Array[Option[InstrInfo]]): Map[Int, Int] = {
        val usedHandlers = instrs.view
            .zip(instrInfos)
            .collect { case (specialized: SpecializedInstr, Some(info)) => (specialized, info) }
            .filter { case (instr, _) => InstructionImpls.getImpl(instr)._2.params.contains(Param.HandlerCheck) }
            .map(_._2.stackInfo.handlers.head.pc)
            .toSet

        val result = mutable.Map.empty[Int, Int]

        for (handlers <- instrInfos.view.flatten.map(_.stackInfo.handlers.view.map(_.pc).reverse.drop(1))) {
            var slot = 0
            for (handler <- handlers if usedHandlers(handler)) {
                result.get(handler) match {
                    case Some(existing) => require(slot == existing)
                    case None => result(handler) = slot
                }
                slot += 1
            }
        }

        result.toMap
    }
}

sealed trait AfterAction

object AfterAction {
    case class PopOperands(n: Int) extends AfterAction
    case class PushHandler(label: Int) extends AfterAction
}

case class Successor(pc: Int, afterActions: Seq[AfterAction])

case class InstrInfo(stackInfo: StackInfo, afterActions: Seq[AfterAction], fallThroughPath: Option[Successor], jumpPaths: Seq[Successor], badPath: Option[Successor]) {
    def goodPaths: Seq[Successor] = fallThroughPath.toSeq ++ jumpPaths

    def allPaths: Seq[Successor] = goodPaths ++ badPath.toSeq
}

object InstrInfo {
    def apply(instr: Instr, pos: Int, possibleStack: Option[StackInfo], canSucceed: Boolean = true, canFail: Boolean = true): Option[InstrInfo] =
        possibleStack match {
            case None =>
                None
            case Some(stackInfo@StackInfo(stacksz, handlers)) =>
                val fallThroughPath = instr.fallThroughPath(stacksz, handlers).filter(_ => canSucceed).map { stackAfter =>
                    Successor(pos + 1, handlerActions(handlers, stackAfter.handlers))
                }
                val jumpPaths = instr.jumpPaths(stacksz, handlers).filter(_ => canSucceed).map { case (target, stackAfter) =>
                    Successor(target, handlerActions(handlers, stackAfter.handlers))
                }
                val badPath = instr.failPath(stacksz, handlers).filter(_ => canFail).map { stackAfter =>
                    val activeHandler = stackAfter.handlers.head
                    Successor(activeHandler.pc, handlerActions(handlers, stackAfter.handlers) ++ popActions(stackAfter.stacksz, activeHandler.stacksz))
                }
                Some(optimizeActions(stackInfo, fallThroughPath, jumpPaths, badPath))
        }

    private def handlerActions(handlersBefore: List[HandlerInfo], handlersAfter: List[HandlerInfo]): Seq[AfterAction] = {
        val pcsAfter = handlersAfter.reverse.view.map(_.pc)

        val matchCount = handlersBefore.reverse
            .view
            .map(_.pc)
            .zip(pcsAfter)
            .takeWhile { case (b, a) => b == a }
            .size

        pcsAfter.drop(matchCount).map(AfterAction.PushHandler(_)).toSeq
    }

    private def popActions(startStack: Int, endStack: Int): Seq[AfterAction] =
        if (endStack >= startStack) {
            Seq.empty
        } else {
            Seq(AfterAction.PopOperands(startStack - endStack))
        }

    private def optimizeActions(stackInfo: StackInfo, fallThroughPathIn: Option[Successor], jumpPathsIn: Seq[Successor], badPathIn: Option[Successor]): InstrInfo = {
        var fallThroughPath = fallThroughPathIn
        var jumpPaths = jumpPathsIn
        var badPath = badPathIn

        val sharedActions = Seq.newBuilder[AfterAction]
        while (true) {
            val heads = (fallThroughPath.view ++ jumpPaths ++ badPath).map(_.afterActions.headOption).toSet.toSeq
            heads match {
                case Seq(Some(sharedAction)) =>
                    sharedActions += sharedAction

                    fallThroughPath = fallThroughPath.map(tailActions)
                    jumpPaths = jumpPaths.map(tailActions)
                    badPath = badPath.map(tailActions)
                case _ =>
                    return InstrInfo(stackInfo, sharedActions.result(), fallThroughPath, jumpPaths, badPath)
            }
        }

        throw new AssertionError("unreachable")
    }

    private def tailActions(successor: Successor): Successor = successor.copy(afterActions = successor.afterActions.tail)
}