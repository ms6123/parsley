package parsley.internal.machine.jit.utils

import scala.collection.JavaConverters.*
import scala.collection.mutable

import org.objectweb.asm.{MethodVisitor, Opcodes}
import org.objectweb.asm.tree.*

private[utils] class DceMethodAdapter(access: Int,
                                      name: String,
                                      desc: String,
                                      signature: String,
                                      exceptions: Array[String],
                                      nextVisitor: MethodVisitor,
                                     ) extends MethodNode(Opcodes.ASM9, access, name, desc, signature, exceptions) {
    override def visitEnd(): Unit = {
        if (instructions.size() > 0) {
            eliminateDeadCode()
        }

        if (nextVisitor != null) {
            accept(nextVisitor)
        }
    }

    private def eliminateDeadCode(): Unit = {
        val reachable = mutable.Set.empty[AbstractInsnNode]
        val worklist = mutable.Stack.empty[AbstractInsnNode]

        val handlersFor = mutable.Map.empty[AbstractInsnNode, List[LabelNode]].withDefaultValue(Nil)
        for (tcb <- tryCatchBlocks.asScala) {
            var curr = tcb.start.asInstanceOf[AbstractInsnNode]
            while (curr != null && curr != tcb.end) {
                handlersFor(curr) = tcb.handler :: handlersFor(curr)
                curr = curr.getNext
            }
        }

        def enqueue(insn: AbstractInsnNode): Unit = {
            if (insn != null && reachable.add(insn)) {
                worklist.push(insn)
            }
        }

        enqueue(instructions.getFirst)

        while (worklist.nonEmpty) {
            val insn = worklist.pop()
            val opcode = insn.getOpcode

            handlersFor(insn).foreach(enqueue)

            insn match {
                case jump: JumpInsnNode =>
                    enqueue(jump.label)
                    if (opcode != Opcodes.GOTO) {
                        enqueue(insn.getNext)
                    }

                case ts: TableSwitchInsnNode =>
                    enqueue(ts.dflt)
                    ts.labels.asScala.foreach(enqueue)

                case ls: LookupSwitchInsnNode =>
                    enqueue(ls.dflt)
                    ls.labels.asScala.foreach(enqueue)

                case _ =>
                    opcode match {
                        case Opcodes.IRETURN | Opcodes.LRETURN | Opcodes.FRETURN |
                             Opcodes.DRETURN | Opcodes.ARETURN | Opcodes.RETURN |
                             Opcodes.ATHROW =>
                        case _ => enqueue(insn.getNext)
                    }
            }
        }

        var curr = instructions.getFirst
        while (curr != null) {
            val next = curr.getNext
            if (!reachable.contains(curr)) {
                curr match {
                    case _: LabelNode | _: LineNumberNode => // Keep
                    case _ => instructions.remove(curr)
                }
            }
            curr = next
        }

        tryCatchBlocks.removeIf(it => it.start == it.end)
    }
}
