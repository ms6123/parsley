package parsley.internal.machine.jit

import parsley.internal.machine.instructions.{HandlerInfo, Instr, StackInfo}

class FunctionInfo(val instrInfos: Array[Option[InstrInfo]], val handlerSlots: Map[Int, Int])

object FunctionInfo {
    def apply(instrInfos: Array[Option[InstrInfo]]): FunctionInfo = {
        val handlerTree = instrInfos.view
            .flatten
            .map(_.stackInfo.handlers.view.map(_.pc))
            .flatMap(it => it.zip(it.tail))
            .groupBy(_._2)
            .view
            .mapValues(_.view.map(_._1).toSet)
            .toMap
        new FunctionInfo(instrInfos, allocateHandlerSlots(handlerTree))
    }

    private def allocateHandlerSlots(handlerTree: Map[Int, Set[Int]]): Map[Int, Int] = {
        val result = Map.newBuilder[Int, Int]

        def go(handler: Int, depth: Int): Unit = {
            result += handler -> depth
            for (child <- handlerTree.getOrElse(handler, Set.empty)) {
                go(child, depth + 1)
            }
        }

        for (start <- handlerTree.get(-1).toSeq.flatten) {
            go(start, 0)
        }
        result.result()
    }
}

enum AfterAction {
    case PopOperands(n: Int)
    case PushHandler(label: Int)
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