package parsley.internal.machine.jit

import org.objectweb.asm.{Label, Opcodes, Type}
import parsley.internal.machine.Context
import parsley.internal.machine.instructions.Instr

private val IS_ENABLED = System.getProperty("parsley.jit.enabled", "false").toBoolean
private val GEN_PACKAGE = "parsley/internal/machine/jit/gen/blocks/"

object Optimizer {
  def optimize(instrs: Array[Instr]): Array[Instr] = {
    if (!IS_ENABLED) {
      return instrs
    }

    val blockStarts = (instrs.view.zipWithIndex.flatMap { case (instr, pos) => instr.labels(pos) }.toSet + 0).toSeq.sorted

    val basicBlocks = {
      val blockEnds = blockStarts.tail.map(_ - 1) :+ (instrs.length - 1)
      blockStarts.zip(blockEnds).map { case (start, end) => BasicBlock(start, end + 1, instrs.slice(start, end + 1)) }
    }

    val blockStartToIndex = basicBlocks.zipWithIndex.map { case (block, idx) => block.start -> idx }.toMap

    for (instr <- instrs) {
      instr.relabel(blockStartToIndex)
    }

    val context = ClassGenContext()
    basicBlocks.view.map(_.build(context)).toArray
  }

  private case class BasicBlock(start: Int, end: Int, instrs: Array[Instr]) {
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
