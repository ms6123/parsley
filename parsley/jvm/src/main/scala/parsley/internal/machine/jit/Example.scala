package parsley.internal.machine.jit

import org.objectweb.asm.Opcodes

@main
def main(): Unit = {
  val ctx = ClassGenContext()
  val clazz = ctx.newClass(Opcodes.ACC_PUBLIC, "Example") { visitor =>
    val main = visitor.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "main", "()Ljava/lang/Object;", null, null)
    main.loadObject(List(1, 2, 3))
    main.loadObject(List(4, 2, 3))
    main.visitInsn(Opcodes.ARETURN)
    main.visitEnd()
  }
  System.err.println(clazz.getMethod("main").invoke(null))
}