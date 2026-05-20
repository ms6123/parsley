package parsley.internal.machine.jit

import scala.annotation.tailrec
import scala.collection.mutable

import parsley.errors.ErrorBuilder

import parsley.internal.machine.{Context, ParseRunner}
import parsley.internal.machine.instructions.*
import parsley.internal.machine.jit.codegen.{ParserFunction, ParserGenerator}

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

        val functions = functionRanges.view.map { funcRange =>
            for (i <- funcRange) {
                instrs(i) match {
                    case _: Call =>
                    case instr => instr.relabel(_ - funcRange.start)
                }
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
                instr.relabel(it => copiedHandlers.getOrElse(it, it))
            }

            tailrecOptimization(funcRange, funcInstrs)

            funcRange.start -> funcInstrs.toArray
        }.toMap

        val analysis = analyzeAll(functions)
        val isCyclic = findCyclicFunctions(functions, analysis)

        val startMethod = new ParserGenerator(
            functionRanges.view.map(_.start).map { id =>
                val tailInstrs = findTailInstrs(functions(id), analysis(id).instrInfos)
                val suspensionPoints = findSuspensionPoints(functions(id), producesResults(id), analysis(id), isCyclic, tailInstrs)

                new ParserFunction(id, functions(id), producesResults(id), isCyclic(id), tailInstrs, suspensionPoints, analysis(id))
            }.toArray
        ).generate()

        new ParseRunner {
            override def run[Err: ErrorBuilder, A](input: String, numRegs: Int, sourceFile: Option[String]): Result[Err, A] =
                new JitContext(startMethod, input, numRegs, sourceFile).run() match {
                    case success: Success[?] => success
                    case Failure(_) =>
                        System.err.println(s"Falling back to interpreter")
                        Context.interpreterRunner(originalInstrs)
                            .run(input, numRegs, sourceFile)
                }

            override def dynCall(ctx: Context, pc: Int): Any =
                ctx match {
                    case ctx: JitContext =>
                        startMethod.invokeExact(ctx)
                }
        }
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

    private def findCyclicFunctions(functions: Map[Int, Array[Instr]], analysis: Map[Int, FunctionInfo]): Set[Int] = {
        val graph = functions.map { case (ourId, instrs) =>
            val instrInfos = analysis(ourId).instrInfos
            ourId -> instrs.zipWithIndex.collect { case (Call(id, _), pos) if instrInfos(pos).isDefined => id }.toSet
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

    private def analyzeAll(funcById: Map[Int, Array[Instr]]): Map[Int, FunctionInfo] = {
        val callers = {
            val m = scala.collection.mutable.Map[Int, Set[Int]]().withDefaultValue(Set.empty)
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

        funcById.view.map { case (id, instrs) =>
            val stacks = analyze(instrs, state).stacks
            val instrInfos = instrs.view.zipWithIndex.map { case (instr, pos) =>
                val (canSucceed, canFail) = instr match {
                    case Call(callId, _) => state(callId)
                    case _ => (true, true)
                }
                InstrInfo(instr, pos, stacks(pos), canSucceed, canFail)
            }.toArray
            id -> FunctionInfo(instrs, instrInfos)
        }.toMap
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

    private def findTailInstrs(instrs: Array[Instr], infos: Array[Option[InstrInfo]]): Set[Int] = {
        val n = instrs.length

        val resolved = Array.tabulate(n) { pc =>
            if (infos(pc).isEmpty || !isNoop(instrs(pc))) Set(pc) else Set.empty[Int]
        }

        def resolveSuccessors(pcs: Seq[Successor]): Set[Int] =
            pcs.view.map(_.pc).flatMap {
                case -1 => Set(-1)
                case pc => resolved(pc)
            }.toSet

        val noopPreds = Array.fill(n)(mutable.Set[Int]())
        for (pc <- 0 until n; info <- infos(pc) if isNoop(instrs(pc)))
            for (succ <- (info.goodPaths ++ info.badPath).map(_.pc) if succ >= 0)
                noopPreds(succ) += pc

        val inWorklist = mutable.Set[Int]()
        val worklist = mutable.Queue[Int]()

        def enqueue(pc: Int): Unit =
            if (inWorklist.add(pc)) worklist.enqueue(pc)

        (0 until n).filter(pc => infos(pc).isDefined && isNoop(instrs(pc))).foreach(enqueue)

        while (worklist.nonEmpty) {
            val pc = worklist.dequeue()
            inWorklist -= pc
            val incoming = resolveSuccessors(infos(pc).get.goodPaths ++ infos(pc).get.badPath)
            if (incoming != resolved(pc)) {
                resolved(pc) = incoming
                noopPreds(pc).foreach(enqueue)
            }
        }

        (0 until n).filter { i =>
            !isNoop(instrs(i)) && (infos(i) match {
                case None => false
                case Some(info) =>
                    val effectiveGood = resolveSuccessors(info.goodPaths)
                    val effectiveBad = resolveSuccessors(info.badPath.toSeq)

                    val goodOk = info.goodPaths.isEmpty ||
                        effectiveGood.forall(pc => pc != -1 && isFunctionTerminator(instrs(pc)))

                    val badOk = info.badPath.isEmpty ||
                        effectiveBad.forall(_ == -1)

                    goodOk && badOk
            })
        }.toSet
    }

    private def isNoop(instr: Instr): Boolean = instr match {
        case instr: SpecializedInstr =>
            val info = InstructionImpls.getImpl(instr)._2
            info.noop && info.consumeOperands == 0 && info.beforeActions.isEmpty && info.afterActions.isEmpty
        case _ => false
    }

    private def findSuspensionPoints(instrs: Array[Instr], producesResults: Boolean, info: FunctionInfo, isCyclic: Int => Boolean, isTail: Int => Boolean): Array[Int] =
        instrs.zipWithIndex.collect {
            case (Call(theirId, theyProduceResults), pos) if
                info.instrInfos(pos).isDefined && isCyclic(theirId) && (!isTail(pos) || producesResults != theyProduceResults) =>
                pos
        }

    private def isFunctionTerminator(instr: Instr): Boolean = instr match {
        case Halt | Return => true
        case _ => false
    }
}

private class AnalysisResult(val canSucceed: Boolean, val canFail: Boolean, val stacks: Array[Option[StackInfo]]) {
    def outcomes: (Boolean, Boolean) = (canSucceed, canFail)
}
