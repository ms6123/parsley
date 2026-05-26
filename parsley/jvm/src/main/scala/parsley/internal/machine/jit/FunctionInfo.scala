package parsley.internal.machine.jit

import scala.collection.mutable

import parsley.internal.machine.instructions.{HandlerInfo, Instr, StackInfo}

class FunctionInfo(val instrInfos: Array[InstrInfo], val handlerSlots: Map[Int, Int])

object FunctionInfo {
    def apply(instrInfos: Array[InstrInfo]): FunctionInfo = {
        new FunctionInfo(instrInfos, allocateHandlerSlots(instrInfos))
    }

    private def allocateHandlerSlots(instrInfos: Array[InstrInfo]): Map[Int, Int] = {
        val usedHandlers = instrInfos.flatMap { info =>
            info.allSuccessors.view.flatMap(_.afterActions.pushHandlers)
        }.toSet
        val result = mutable.Map.empty[Int, Int]

        for (handlers <- instrInfos.view.map(_.stackInfo.handlers.view.map(_.pc).reverse.drop(1))) {
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

case class AfterActions(popOperands: Int = 0, pushHandlers: Set[Int] = Set.empty) {
    def isEmpty: Boolean = popOperands == 0 && pushHandlers.isEmpty

    def ++(other: AfterActions): AfterActions = copy(popOperands = popOperands + other.popOperands, pushHandlers = pushHandlers ++ other.pushHandlers)

    def filterHandlers(isUsed: Int => Boolean): AfterActions = copy(pushHandlers = pushHandlers.filter(isUsed))
}

object AfterActions {
    val empty: AfterActions = AfterActions()

    def combine(actions: Seq[AfterActions]): AfterActions = actions.foldLeft(AfterActions())(_ ++ _)
}

case class Successor(pc: Int, afterActions: AfterActions) {
    def relabel(labels: PartialFunction[Int, Int]): Successor = copy(pc = if (pc == -1) -1 else labels(pc))

    def filterHandlers(isUsed: Int => Boolean): Successor = copy(afterActions = afterActions.filterHandlers(isUsed))
}

case class IndicatedSuccessor(indicator: Int, successor: Successor) {
    def relabel(labels: PartialFunction[Int, Int]): IndicatedSuccessor = copy(successor = successor.relabel(labels))

    def filterHandlers(isUsed: Int => Boolean): IndicatedSuccessor = copy(successor = successor.filterHandlers(isUsed))
}

case class InstrInfo(stackInfo: StackInfo, fallThroughPath: Option[Successor], jumpPaths: Seq[IndicatedSuccessor], badPath: Option[Successor]) {
    def goodPaths(pos: Int): Seq[IndicatedSuccessor] = fallThroughPath.map(IndicatedSuccessor(pos + 1, _)).toSeq ++ jumpPaths

    def allPaths(pos: Int): Seq[IndicatedSuccessor] = goodPaths(pos) ++ badPath.map(IndicatedSuccessor(-1, _)).toSeq

    def allSuccessors: Seq[Successor] = fallThroughPath.toSeq ++ jumpPaths.map(_.successor) ++ badPath.toSeq

    def relabel(labels: PartialFunction[Int, Int]): InstrInfo = {
        InstrInfo(
            stackInfo,
            fallThroughPath.map(_.relabel(labels)),
            jumpPaths.map(_.relabel(labels)),
            badPath.map(_.relabel(labels)),
        )
    }

    def filterHandlers(isUsed: Int => Boolean): InstrInfo = {
        InstrInfo(
            stackInfo,
            fallThroughPath.map(_.filterHandlers(isUsed)),
            jumpPaths.map(_.filterHandlers(isUsed)),
            badPath.map(_.filterHandlers(isUsed)),
        )
    }
}

object InstrInfo {
    def apply(instr: Instr, pos: Int, possibleStack: Option[StackInfo], canSucceed: Boolean = true, canFail: Boolean = true): Option[InstrInfo] =
        possibleStack match {
            case None =>
                None
            case Some(stackInfo@StackInfo(stacksz, handlers)) =>
                val fallThroughPath = instr.fallThroughPath(stacksz, handlers).filter(_ => canSucceed).map { stackAfter =>
                    Successor(pos + 1, AfterActions(pushHandlers = pushedHandlers(handlers, stackAfter.handlers)))
                }
                val jumpPaths = instr.jumpPaths(stacksz, handlers).filter(_ => canSucceed).map { case (target, stackAfter) =>
                    IndicatedSuccessor(target, Successor(target, AfterActions(pushHandlers = pushedHandlers(handlers, stackAfter.handlers))))
                }
                val badPath = instr.failPath(stacksz, handlers).filter(_ => canFail).map { stackAfter =>
                    val activeHandler = stackAfter.handlers.head
                    Successor(
                        activeHandler.pc,
                        AfterActions(poppedOperands(stackAfter.stacksz, activeHandler.stacksz), pushedHandlers(handlers, stackAfter.handlers)),
                    )
                }
                Some(InstrInfo(stackInfo, fallThroughPath, jumpPaths, badPath))
        }

    private def pushedHandlers(handlersBefore: List[HandlerInfo], handlersAfter: List[HandlerInfo]): Set[Int] = {
        val pcsAfter = handlersAfter.reverse.view.map(_.pc)

        val matchCount = handlersBefore.reverse
            .view
            .map(_.pc)
            .zip(pcsAfter)
            .takeWhile { case (b, a) => b == a }
            .size

        pcsAfter.drop(matchCount).toSet
    }

    private def poppedOperands(startStack: Int, endStack: Int): Int = math.max(0, startStack - endStack)
}