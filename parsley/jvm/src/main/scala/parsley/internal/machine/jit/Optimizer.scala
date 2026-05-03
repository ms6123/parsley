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

        val functions = functionRanges.map { funcRange =>
            for (i <- funcRange) {
                instrs(i) match {
                    case Call(_) =>
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

            ParserFunction(funcRange.start, funcInstrs.toArray, determineSuccessors(funcInstrs))
        }

        val startMethod = ParserGenerator(functions.toArray).generate()

        new ParseRunner {
            override def run[Err: ErrorBuilder, A](input: String, numRegs: Int, sourceFile: Option[String]): Result[Err, A] =
                JitContext(startMethod, input, numRegs, sourceFile).run() match {
                    case success: Success[?] => success
                    case Failure(_) =>
                        System.err.println(s"Falling back to interpreter")
                        Context.interpreterRunner(originalInstrs)
                            .run(input, numRegs, sourceFile)
                }

            override def dynCall(ctx: Context, pc: Int): Int =
                ctx match {
                    case ctx: JitContext =>
                        //noinspection ScalaUnusedExpression
                        startMethod.invokeExact(ctx): Boolean
                        if (ctx.good) pc + 1 else -1
                }
        }
    }

    private def tailrecOptimization(funcRange: Range, instrs: mutable.ArrayBuffer[Instr]): Unit = {
        for (i <- 0 until instrs.indices.last) {
            (instrs(i), instrs(i + 1)) match {
                case (Call(callId), Return) if callId == funcRange.start =>
                    instrs(i) = Jump(0)
                case _ =>
            }
        }
    }

    private def determineSuccessors(instrs: mutable.ArrayBuffer[Instr]): Array[SuccessorInfo] = {
        val visited = Array.fill(instrs.length)(mutable.Set.empty[List[Int]])
        val toVisit = mutable.Queue(List(-1) -> 0)

        visited(0) += List(-1)

        while (toVisit.nonEmpty) {
            val (handlers, pos) = toVisit.dequeue()

            for ((nextHandlers, nextPos) <- instrs(pos).allPaths(handlers, pos)) {
                if (nextPos != -1 && visited(nextPos).add(nextHandlers)) {
                    toVisit.enqueue(nextHandlers -> nextPos)
                }
            }
        }

        instrs.view.zip(visited).zipWithIndex.map { case ((instr, possibleHandlers), pos) =>
            SuccessorInfo(instr, possibleHandlers, pos)
        }.toArray
    }
}
