package parsley.internal.machine.jit

import scala.collection.mutable

import parsley.internal.machine.instructions.{HandlerInfo, Instr, SpecializedInstr, StackInfo}
import parsley.internal.machine.instructions.JitImpl.Param

class FunctionInfo(val instrInfos: Array[InstrInfo], val handlerSlots: Map[Int, Int])

object FunctionInfo {
    def apply(instrs: Array[Instr], instrInfos: Array[InstrInfo]): FunctionInfo = {
        new FunctionInfo(instrInfos, allocateHandlerSlots(instrs, instrInfos))
    }

    private def allocateHandlerSlots(instrs: Array[Instr], instrInfos: Array[InstrInfo]): Map[Int, Int] = {
        val usedHandlers = instrs.view
            .zip(instrInfos)
            .collect { case (specialized: SpecializedInstr, info) => (specialized, info) }
            .filter { case (instr, _) => InstructionImpls.getImpl(instr)._2.params.contains(Param.HandlerCheck) }
            .map(_._2.stackInfo.handlers.head.pc)
            .toSet

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

    def relabel(labels: PartialFunction[Int, Int]): AfterActions = copy(pushHandlers = pushHandlers.flatMap(labels.lift(_)))

    def ++(other: AfterActions): AfterActions = copy(popOperands = popOperands + other.popOperands, pushHandlers = pushHandlers ++ other.pushHandlers)
}

object AfterActions {
    val empty: AfterActions = AfterActions()

    def combine(actions: Seq[AfterActions]): AfterActions = actions.foldLeft(AfterActions())(_ ++ _)
}

case class Successor(pc: Int, afterActions: AfterActions) {
    def relabel(labels: PartialFunction[Int, Int]): Successor = copy(pc = if (pc == -1) -1 else labels(pc), afterActions.relabel(labels))
}

case class InstrInfo private(stackInfo: StackInfo, afterActions: AfterActions, fallThroughPath: Option[Successor], jumpPaths: Seq[Successor], badPath: Option[Successor]) {
    def goodPaths: Seq[Successor] = fallThroughPath.toSeq ++ jumpPaths

    def allPaths: Seq[Successor] = goodPaths ++ badPath.toSeq

    def relabel(labels: PartialFunction[Int, Int]): InstrInfo = {
        def relabelSuccessor(successor: Successor) = successor.copy(afterActions = afterActions ++ successor.afterActions).relabel(labels)

        InstrInfo(
            stackInfo.relabel(labels),
            fallThroughPath.map(relabelSuccessor),
            jumpPaths.map(relabelSuccessor),
            badPath.map(relabelSuccessor),
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
                    Successor(target, AfterActions(pushHandlers = pushedHandlers(handlers, stackAfter.handlers)))
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

    def apply(stackInfo: StackInfo, fallThroughPath: Option[Successor], jumpPaths: Seq[Successor], badPath: Option[Successor]): InstrInfo = {
        val allPaths = fallThroughPath.toSeq ++ jumpPaths ++ badPath.toSeq
        if (allPaths.isEmpty) {
            InstrInfo(stackInfo, AfterActions(), fallThroughPath, jumpPaths, badPath)
        } else {
            val sharedPops = allPaths.map(_.afterActions.popOperands).min
            val sharedHandlers = allPaths.map(_.afterActions.pushHandlers).reduce(_ & _)

            def adjustSuccessor(successor: Successor): Successor = {
                val afterActions = successor.afterActions
                successor.copy(afterActions = AfterActions(afterActions.popOperands - sharedPops, afterActions.pushHandlers -- sharedHandlers))
            }

            InstrInfo(
                stackInfo,
                AfterActions(sharedPops, sharedHandlers),
                fallThroughPath.map(adjustSuccessor),
                jumpPaths.map(adjustSuccessor),
                badPath.map(adjustSuccessor),
            )
        }
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