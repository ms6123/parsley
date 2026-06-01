package parsley.internal.machine.jit

import scala.annotation.tailrec
import scala.collection.mutable

import parsley.errors.ErrorBuilder

import parsley.internal.machine.{Context, ParseRunner}
import parsley.internal.machine.instructions.*
import parsley.internal.machine.instructions.JitImpl.Param
import parsley.internal.machine.jit.codegen.{JitParseRunner, ParserFunction, ParserGenerator}
import parsley.internal.machine.jit.utils.CycleBreaker

import parsley.{Failure, Result, Success}

object Optimizer {
    private val IS_ENABLED: Boolean = System.getProperty("parsley.jit.enabled", "true").toBoolean

    val useTco: Boolean = !IS_ENABLED
    val allowInlining: Boolean = !IS_ENABLED

    def optimize(originalInstrs: Array[Instr]): ParseRunner = {
        if (!IS_ENABLED) {
            System.err.println(s"Interpreting ${originalInstrs.length} instructions")
            return Context.interpreterRunner(originalInstrs)
        }

        System.err.println(s"JITing ${originalInstrs.length} instructions")

        val instrs = originalInstrs.map(_.copy)

        val functionRanges = mutable.ArrayBuffer[Range]()
        var chunkStart = 0

        for (i <- instrs.indices) {
            if (isFunctionTerminator(instrs(i))) {
                functionRanges += chunkStart to i
                chunkStart = i + 1
            }
        }

        val producesResults = (instrs.iterator.collect {
            case Call(id, producesResults) => id -> producesResults
        } ++ Iterator.single(0 -> true)).toMap

        val functions = mutable.Map.empty[Int, Array[Instr]] ++= functionRanges.view.map { funcRange =>
            for (i <- funcRange) {
                relabelLocal(instrs(i))(_ - funcRange.start)
            }

            val funcInstrs = mutable.ArrayBuffer.empty[Instr] ++= instrs.view.slice(funcRange.start, funcRange.last + 1)
            val copiedHandlers = mutable.Map.empty[Int, Int]

            for (instr <- funcRange.view.map(instrs) if !instr.isInstanceOf[Call]) {
                for (label <- instr.labels if label >= funcRange.length) {
                    val foreignTarget = instrs(label + funcRange.start)
                    require(foreignTarget.isInstanceOf[RefailInstr])

                    copiedHandlers.getOrElseUpdate(label, {
                        funcInstrs += foreignTarget
                        funcInstrs.indices.last
                    })
                }
                relabelLocal(instr)(it => copiedHandlers.getOrElse(it, it))
            }

            tailrecOptimization(funcRange, funcInstrs)

            funcRange.start -> funcInstrs.toArray
        }

        val analysis = analyzeAll(functions)
        val isCyclic = findCyclicFunctions(functions)
        val parserFunctions = functionRanges.view.map(_.start).map { id =>
            val tailInstrs = findTailInstrs(functions(id), analysis(id).instrInfos)
            val suspensionPoints = findSuspensionPoints(functions(id), producesResults(id), isCyclic, tailInstrs)

            new ParserFunction(id, functions(id), producesResults(id), isCyclic(id), tailInstrs, suspensionPoints, analysis(id))
        }.toArray

        breakPassthroughCycles(parserFunctions)

        new JitParseRunner(parserFunctions, originalInstrs)
    }

    private def tailrecOptimization(funcRange: Range, instrs: mutable.ArrayBuffer[Instr]): Unit = {
        for (i <- 0 until instrs.indices.last) {
            (instrs(i), instrs(i + 1)) match {
                case (Call(callId, _), Return) if callId == funcRange.start =>
                    instrs(i) = new Jump(0)
                case _ =>
            }
        }
    }

    private def findCyclicFunctions(functions: collection.Map[Int, Array[Instr]]): Set[Int] = {
        val graph = functions.map { case (ourId, instrs) =>
            ourId -> instrs.collect {
                case Call(id, _) => id
                case DynCall(_) => 0 // Pretend each DynCall calls the root parser, explained in paper
            }.toSet
        }
        val visited = mutable.Set.empty[Int]
        val onStack = mutable.Set.empty[Int]
        val inCycle = Set.newBuilder[Int]

        def dfs(v: Int, path: List[Int]): Unit = {
            if (onStack(v)) {
                // mark the cycle portion of the path
                val idx = path.indexOf(v)
                if (idx >= 0) inCycle ++= path.drop(idx)
                return
            }

            if (visited(v)) return

            visited += v
            onStack += v

            for (w <- graph.getOrElse(v, Set.empty)) {
                dfs(w, path :+ v)
            }

            onStack -= v
        }

        for (v <- graph.keys) {
            if (!visited(v)) dfs(v, Nil)
        }

        inCycle.result()
    }

    private def analyzeAll(funcById: mutable.Map[Int, Array[Instr]]): Map[Int, FunctionInfo] = {
        val callers = {
            val m = mutable.Map[Int, Set[Int]]().withDefaultValue(Set.empty)
            for ((callerId, instrs) <- funcById; case Call(id, _) <- instrs.filter(_.isInstanceOf[Call])) {
                m(id) = m(id) + callerId
            }
            m.toMap.withDefaultValue(Set.empty)
        }

        val state = mutable.Map.empty[Int, (Boolean, Boolean)] ++= funcById.keySet.view.map(_ -> (false, false))
        val inQueue = mutable.Set.empty[Int] ++= funcById.keySet
        val queue = mutable.Queue.empty[Int] ++= funcById.keySet

        while (queue.nonEmpty) {
            val id = queue.dequeue()
            inQueue -= id
            val next = analyze(funcById(id), state).outcomes
            if (next != state(id)) {
                state(id) = next
                for (caller <- callers(id) if !inQueue(caller)) {
                    inQueue += caller
                    queue.enqueue(caller)
                }
            }
        }

        funcById.toMap.map { case (id, instrs) =>
            val stacks = analyze(instrs, state).stacks
            val instrInfos = instrs.zipWithIndex.map { case (instr, pos) =>
                val (canSucceed, canFail) = instr match {
                    case Call(callId, _) => state(callId)
                    case _ => (true, true)
                }
                InstrInfo(instr, pos, stacks(pos), canSucceed, canFail)
            }

            val (reachableInstrs, reachableInfos) = pruneUnreachable(instrs, instrInfos)
            val (finalInstrs, finalInfos) = iterativelyCollapseNoops(reachableInstrs, reachableInfos)

            funcById(id) = finalInstrs

            id -> FunctionInfo(finalInfos)
        }
    }

    private def analyze(instrs: Array[Instr], knownResults: Int => (Boolean, Boolean)): AnalysisResult = {
        val visited = Array.fill(instrs.length)(mutable.Set.empty[StackInfo])
        val toVisit = mutable.Queue(0 -> StackInfo(0, List(HandlerInfo(-1, 0))))

        visited(0) += StackInfo(0, List(HandlerInfo(-1, 0)))

        var canSucceed = false
        var canFail = false

        while (toVisit.nonEmpty) {
            val (pos, StackInfo(stacksz, handlers)) = toVisit.dequeue()
            val instr = instrs(pos)

            val paths = instr match {
                case Return | Halt =>
                    canSucceed = true
                    Seq()
                case Call(id, _) =>
                    knownResults(id) match {
                        case (true, true) => instr.allPaths(stacksz, handlers, pos)
                        case (true, false) => instr.goodPaths(stacksz, handlers, pos)
                        case (false, true) => instr.badPaths(stacksz, handlers)
                        case (false, false) => Seq()
                    }
                case _ =>
                    instr.allPaths(stacksz, handlers, pos)
            }

            for ((nextPos, nextStack) <- paths) {
                if (nextPos == -1) {
                    canFail = true
                } else if (visited(nextPos).add(nextStack)) {
                    require(visited(nextPos).size == 1, s"Stack mismatch at $nextPos: ${visited(nextPos)}")
                    toVisit.enqueue(nextPos -> nextStack)
                }
            }
        }

        new AnalysisResult(canSucceed, canFail, visited.map(_.headOption))
    }

    private def pruneUnreachable(instrs: Array[Instr], infos: Array[Option[InstrInfo]]): (Array[Instr], Array[InstrInfo]) = {
        val indexMap = infos
            .zipWithIndex
            .collect { case (Some(_), originalIndex) => originalIndex }
            .zipWithIndex
            .map { case (originalIndex, finalIndex) => originalIndex -> finalIndex }
            .toMap

        instrs.zip(infos)
            .collect { case (instr, Some(info)) => (instr, info.relabel(indexMap)) }
            .unzip
    }

    private def iterativelyCollapseNoops(instrs: Array[Instr], infos: Array[InstrInfo]): (Array[Instr], Array[InstrInfo]) = {
        var (resInstrs, resInfos) = (instrs, infos)
        while ({
            val oldLength = resInstrs.length
            val usedHandlers = findUsedHandlers(resInstrs, resInfos)
            val res = collapseNoops(resInstrs, resInfos.map(_.filterHandlers(usedHandlers)))
            resInstrs = res._1
            resInfos = res._2
            resInstrs.length != oldLength
        }) {}
        (resInstrs, resInfos)
    }

    private def collapseNoops(instrs: Array[Instr], infos: Array[InstrInfo]): (Array[Instr], Array[InstrInfo]) = {
        val collapsedDestinations = mutable.Map.empty[Int, Int]

        @tailrec def go(originalPc: Int, pc: Int, afterActions: AfterActions): Successor = {
            lazy val successor = findNoopSuccessor(instrs(pc), infos(pc))
            if (pc == -1 || successor.isEmpty) {
                collapsedDestinations(originalPc) = pc
                Successor(pc, afterActions)
            }
            else {
                go(originalPc, successor.get.pc, afterActions ++ successor.get.afterActions)
            }
        }

        def collapseSuccessor(successor: Successor): Successor =
            go(successor.pc, successor.pc, successor.afterActions)

        val finalIndices = instrs.indices.filter(it => it == 0 || findNoopSuccessor(instrs(it), infos(it)).isEmpty)

        val collapsed = finalIndices.map { pos =>
            val instr = instrs(pos)
            val InstrInfo(stackInfo, fallThroughPath, jumpPaths, badPath) = infos(pos)

            instr -> InstrInfo(
                stackInfo,
                fallThroughPath.map(collapseSuccessor),
                jumpPaths.map(it => it.copy(successor = collapseSuccessor(it.successor))),
                badPath.map(collapseSuccessor),
            )
        }

        val indexMap = finalIndices.zipWithIndex.toMap

        val labels = Function.unlift { (label: Int) =>
            indexMap.get(collapsedDestinations.getOrElse(label, label))
        }

        collapsed.map { case (instr, info) =>
            instr -> info.relabel(labels)
        }.toArray.unzip
    }

    private def findNoopSuccessor(instr: Instr, instrInfo: InstrInfo): Option[Successor] = instr match {
        case instr: SpecializedInstr =>
            val implInfo = InstructionImpls.getImpl(instr)._2
            if (!implInfo.noop || implInfo.beforeActions.nonEmpty || implInfo.afterActions.nonEmpty) {
                None
            } else {
                val possible = instrInfo.allSuccessors.toSet
                if (possible.size == 1) {
                    val res = possible.head
                    Some(res.copy(afterActions = res.afterActions ++ AfterActions(popOperands = implInfo.consumeOperands)))
                } else {
                    None
                }
            }
        case _ => None
    }

    private def findUsedHandlers(instrs: Array[Instr], instrInfos: Array[InstrInfo]): Set[Int] = {
        instrs.view
            .zip(instrInfos)
            .collect { case (specialized: SpecializedInstr, info) => (specialized, info) }
            .filter { case (instr, _) => InstructionImpls.getImpl(instr)._2.params.contains(Param.HandlerCheck) }
            .map(_._2.stackInfo.handlers.head.pc)
            .toSet
    }

    private def findTailInstrs(instrs: Array[Instr], instrInfos: Array[InstrInfo]): Set[Int] = {
        instrInfos.indices.filter { pos =>
            val info = instrInfos(pos)
            info.goodPaths(pos).forall(succ => instrs.lift(succ.successor.pc).exists(isFunctionTerminator) && succ.successor.afterActions.popOperands == 0) &&
                info.badPath.forall(_.pc == -1)
        }.toSet
    }

    private def findSuspensionPoints(instrs: Array[Instr], producesResults: Boolean, isCyclic: Int => Boolean, isTail: Int => Boolean): Array[Int] =
        instrs.zipWithIndex.collect {
            case (Call(theirId, theyProduceResults), pos) if isCyclic(theirId) && (!isTail(pos) || producesResults != theyProduceResults) =>
                pos
            case (_: DynCall, pos) => pos
        }

    private def breakPassthroughCycles(functions: Array[ParserFunction]): Unit = {
        val functionsById = functions.view.map(it => it.id -> it).toMap

        val graph = functions.view
            .filter(_.isPassthrough)
            .map(it => it.id -> it.instrs.collect { case Call(id, _) if functionsById(id).isPassthrough => id }.toSet)
            .toMap
        for (toBreak <- CycleBreaker.chooseBreakNodes(graph)) {
            functionsById(toBreak).isPassthrough = false
        }
    }

    private def isFunctionTerminator(instr: Instr): Boolean = instr match {
        case Halt | Return => true
        case _ => false
    }

    private def relabelLocal[T <: Instr](instr: T)(labels: Int => Int): T = instr match {
        case _: Call => instr
        case instr => instr.relabel(labels)
    }
}

private class AnalysisResult(val canSucceed: Boolean, val canFail: Boolean, val stacks: Array[Option[StackInfo]]) {
    def outcomes: (Boolean, Boolean) = (canSucceed, canFail)
}
