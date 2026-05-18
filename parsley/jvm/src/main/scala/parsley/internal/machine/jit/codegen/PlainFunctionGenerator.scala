package parsley.internal.machine.jit.codegen

import parsley.internal.machine.jit.{ClassGenContext, InstrInfo}
import parsley.internal.machine.jit.codegen.FunctionGenerator.Constants.IMPL_NAME
import parsley.internal.machine.jit.codegen.FunctionGenerator.implDesc

import org.objectweb.asm.Opcodes

class PlainFunctionGenerator(function: ParserFunction, ctx: ParserGenerator, classVisitor: ClassGenContext#ClassGenVisitor)
    extends FunctionGenerator(function, ctx, classVisitor) {

    override protected val vis: ClassGenContext#MethodGenVisitor =
        classVisitor.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, IMPL_NAME, implDesc(function.producesResults), null, null)

    override protected def generateCall(pos: Int, instrInfo: InstrInfo, id: Int, producesResults: Boolean): Unit = {
        loadContext()
        vis.visitMethodInsn(Opcodes.INVOKESTATIC, ParserGenerator.className(id), IMPL_NAME, implDesc(producesResults), false)
        performAfterActions(instrInfo.afterActions)
        jumpUsingReturnValue(pos, instrInfo, if (producesResults) classOf[AnyRef] else classOf[Boolean])
    }
}
