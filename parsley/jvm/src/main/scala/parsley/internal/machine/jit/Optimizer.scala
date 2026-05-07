package parsley.internal.machine.jit

import scala.collection.mutable

import parsley.errors.ErrorBuilder

import parsley.internal.machine.{Context, ParseRunner}
import parsley.internal.machine.instructions.*

import parsley.{Failure, Result, Success}

private val IS_ENABLED = System.getProperty("parsley.jit.enabled", "true").toBoolean
private val GEN_PACKAGE = "parsley/internal/machine/jit/gen/blocks/"

object Optimizer {
    val useTco: Boolean = !IS_ENABLED
    val allowInlining: Boolean = !IS_ENABLED

    def optimize(originalInstrs: Array[Instr]): ParseRunner = {
        if (!IS_ENABLED) {
            System.err.println(s"Interpreting ${originalInstrs.length} instructions")
            return Context.interpreterRunner(originalInstrs)
        }

        System.err.println(s"JITing ${originalInstrs.length} instructions")

        val instrs = originalInstrs.map(_.copy)

        def isFunctionTerminator(instr: Instr): Boolean = instr match {
            case Halt | Return => true
            case _ => false
        }

        val functionRanges = mutable.ArrayBuffer[Range]()
        var chunkStart = 0

        for (i <- instrs.indices) {
            if (isFunctionTerminator(instrs(i))) {
                functionRanges += chunkStart to i
                chunkStart = i + 1
            }
        }

        val producesResults = instrs.view.collect {
            case Call(id, producesResults) => id -> producesResults
        }.concat(Seq(0 -> true)).toMap

        val functions = functionRanges.view.map { funcRange =>
            for (i <- funcRange) {
                instrs(i) match {
                    case _: Call =>
                    case instr => instr.relabel(_ - funcRange.start)
                }
            }

            val funcInstrs = mutable.ArrayBuffer.from(instrs.view.slice(funcRange.start, funcRange.last + 1))
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

        val startMethod = ParserGenerator(
            functionRanges.view.map(_.start).map(id => ParserFunction(id, functions(id), producesResults(id), analysis(id))).toArray
        ).generate()

        new ParseRunner {
            override def run[Err: ErrorBuilder, A](input: String, numRegs: Int, sourceFile: Option[String]): Result[Err, A] =
                JitContext(startMethod, input, numRegs, sourceFile).run() match {
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
                    instrs(i) = Jump(0)
                case _ =>
            }
        }
    }

    private def analyzeAll(funcById: Map[Int, Array[Instr]]): Map[Int, FunctionInfo] = {
        for (instrs <- funcById.values; instr <- instrs) {
            instr match {
                case _: (ManyUntil | Case) =>
                case instr: SpecializedInstr => InstructionImpls.getImpl(instr)
                case _ =>
            }
        }

        val callers = {
            val m = scala.collection.mutable.Map[Int, Set[Int]]().withDefaultValue(Set.empty)
            for ((callerId, instrs) <- funcById; case Call(id, _) <- instrs) {
                m(id) = m(id) + callerId
            }
            m.toMap.withDefaultValue(Set.empty)
        }

        val state = mutable.Map.from(funcById.keySet.view.map(_ -> (false, false)))
        val inQueue = mutable.Set.from(funcById.keySet)
        val queue = mutable.Queue.from(funcById.keySet)

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
            id -> new FunctionInfo(instrInfos)
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

        AnalysisResult(canSucceed, canFail, visited.map(_.headOption))
    }
}

private class AnalysisResult(val canSucceed: Boolean, val canFail: Boolean, val stacks: Array[Option[StackInfo]]) {
    def outcomes: (Boolean, Boolean) = (canSucceed, canFail)
}
