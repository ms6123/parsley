package parsley.internal.machine.jit

import scala.collection.mutable

import parsley.internal.machine.Context
import parsley.internal.machine.instructions.*

import org.objectweb.asm.{Label, Opcodes, Type}

private val IS_ENABLED = System.getProperty("parsley.jit.enabled", "false").toBoolean
private val GEN_PACKAGE = "parsley/internal/machine/jit/gen/blocks/"

object Optimizer {
    val useTco = false
    
    def optimize(instrs: Array[Instr]): Array[Instr] = {
        if (!IS_ENABLED) {
            return instrs
        }

        def isFunctionTerminator(instr: Instr): Boolean = instr match {
            case Halt | Return => true
            case _ => false
        }

        val functions = mutable.ArrayBuffer[ParserFunction]()
        var chunkStart = 0

        for (i <- instrs.indices) {
            if (isFunctionTerminator(instrs(i))) {
                functions += ParserFunction(chunkStart, i + 1, mutable.ArrayBuffer.from(instrs.view.slice(chunkStart, i + 1)))
                chunkStart = i + 1
            }
        }

        val sharedHandlers = chunkStart until instrs.length

        for (func <- functions) {
            for (instr <- func.instrs if !instr.isInstanceOf[Call]) {
                instr.relabel(_ - func.start)
            }

            val copiedHandlers = mutable.Map.empty[Instr, Int]
            for (instr <- func.instrs.clone() if !instr.isInstanceOf[Call]; label <- instr.labels if label >= func.end) {
                val foreignTarget = instrs(label + func.start)
                require(foreignTarget.isInstanceOf[RefailInstr])

                val copiedIndex = copiedHandlers.getOrElseUpdate(foreignTarget, {
                    func.instrs += foreignTarget
                    func.instrs.indices.last
                })
                instr.relabel(it => if (it == label) copiedIndex else it)
            }

            val successorInfos = determineSuccessors(func)
            println()
        }

        instrs
    }

    private def determineSuccessors(func: ParserFunction): Array[SuccessorInfo] = {
        val visited = Array.fill(func.instrs.length)(mutable.Set.empty[List[Int]])
        val toVisit = mutable.Queue(List(-1) -> 0)

        visited(0) += List(-1)

        while (toVisit.nonEmpty) {
            val (handlers, pos) = toVisit.dequeue()

            for ((nextHandlers, nextPos) <- func.instrs(pos).allPaths(handlers, pos)) {
                if (nextPos != -1 && visited(nextPos).add(nextHandlers)) {
                    toVisit.enqueue(nextHandlers -> nextPos)
                }
            }
        }

        func.instrs.view.zip(visited).zipWithIndex.map { case ((instr, possibleHandlers), pos) =>
            SuccessorInfo(instr, possibleHandlers, pos)
        }.toArray
    }

    private case class SuccessorInfo(goodPaths: Set[Int], badPaths: Set[Int])

    private object SuccessorInfo {
        def apply(instr: Instr, possibleHandlers: Iterable[List[Int]], pos: Int): SuccessorInfo = {
            val goodPaths = Set.newBuilder[Int]
            val badPaths = Set.newBuilder[Int]

            for (handlers <- possibleHandlers) {
                if (instr.fallThroughPath(handlers).isDefined) {
                    goodPaths += (pos + 1)
                }
                goodPaths ++= instr.jumpPaths(handlers).map(_._2)
                badPaths ++= instr.failPath(handlers).map(_.head)
            }

            SuccessorInfo(goodPaths.result(), badPaths.result())
        }
    }

    private case class ParserFunction(start: Int, end: Int, instrs: mutable.IndexedBuffer[Instr]) {
        def build(ctx: ClassGenContext): Instr =
            if (instrs.lengthIs == 1) {
                instrs.head
            } else {
                ctx.newClass(Opcodes.ACC_PUBLIC, GEN_PACKAGE + s"BasicBlock$start", Type.getInternalName(classOf[Instr])) { visitor =>
                    buildClass(visitor)
                }.getConstructor().newInstance().asInstanceOf[Instr]
            }

        private def buildClass(visitor: ClassGenContext#ClassGenVisitor): Unit = {
            {
                val ctor = visitor.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null)
                ctor.visitVarInsn(Opcodes.ALOAD, 0)
                ctor.visitMethodInsn(Opcodes.INVOKESPECIAL, Type.getInternalName(classOf[Instr]), "<init>", "()V", false)
                ctor.visitInsn(Opcodes.RETURN)
                ctor.visitEnd()
            }
            {
                val applyDesc = Type.getMethodDescriptor(classOf[Instr].getMethod("apply", classOf[Context]))
                val apply = visitor.visitMethod(Opcodes.ACC_PUBLIC, "apply", applyDesc, null, null)
                val divergedLabel = Label()
                for (instr <- instrs) {
                    apply.loadObject(instr)
                    apply.visitVarInsn(Opcodes.ALOAD, 1)
                    apply.visitMethodInsn(Opcodes.INVOKEVIRTUAL, Type.getInternalName(classOf[Instr]), "apply", applyDesc, false)
                    apply.visitJumpInsn(Opcodes.IFEQ, divergedLabel)
                }
                apply.visitInsn(Opcodes.ICONST_1)
                apply.visitInsn(Opcodes.IRETURN)

                apply.visitLabel(divergedLabel)
                apply.visitInsn(Opcodes.ICONST_0)
                apply.visitInsn(Opcodes.IRETURN)

                apply.visitEnd()
            }
        }
    }
}
