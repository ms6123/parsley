package parsley.internal.machine.jit

import scala.collection.mutable

import parsley.internal.machine.{Context, ParseRunner}
import parsley.internal.machine.instructions.*

private val IS_ENABLED = System.getProperty("parsley.jit.enabled", "true").toBoolean
private val GEN_PACKAGE = "parsley/internal/machine/jit/gen/blocks/"

object Optimizer {
    val useTco: Boolean = !IS_ENABLED

    def optimize(instrs: Array[Instr]): ParseRunner = {
        if (!IS_ENABLED) {
            System.err.println(s"Interpreting ${instrs.length} instructions")
            return Context.interpreterRunner(instrs)
        }

        System.err.println(s"JITing ${instrs.length} instructions")

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
            val copiedHandlers = mutable.Map.empty[Instr, Int]

            for (instr <- funcRange.map(instrs); label <- instr.labels if label >= funcRange.length) {
                val foreignTarget = instrs(label + funcRange.start)
                require(foreignTarget.isInstanceOf[RefailInstr])

                val copiedIndex = copiedHandlers.getOrElseUpdate(foreignTarget, {
                    funcInstrs += foreignTarget
                    funcInstrs.indices.last
                })
                instr.relabel(it => if (it == label) copiedIndex else it)
            }

            ParserFunction(funcRange.start, funcInstrs.toArray, determineSuccessors(funcInstrs))
        }

        ParserGenerator(functions.toSeq).generate()
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
