/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.jit.codegen

import parsley.internal.machine.instructions.{FailMarker, JitImpl}
import parsley.internal.machine.jit.{InstrInfo}
import parsley.internal.machine.jit.codegen.FunctionGenerator.Constants.IMPL_NAME

import org.objectweb.asm.Opcodes

class PlainFunctionGenerator(function: ParserFunction, classVisitor: ClassGenContext#ClassGenVisitor)
    extends FunctionGenerator(function, classVisitor) {

    override protected val implName: String = IMPL_NAME
    override protected val implDesc: String = FunctionGenerator.implDesc(function.producesResults)
    override protected def implIsStatic: Boolean = true
    override protected def contextIndex: Int = 0

    override protected def generateCall(pos: Int, instrInfo: InstrInfo, id: Int, producesResults: Boolean)(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        loadContext()
        vis.visitMethodInsn(Opcodes.INVOKESTATIC, ParserGenerator.className(id), IMPL_NAME, FunctionGenerator.implDesc(producesResults), false)
        jumpUsingReturnValue(pos, instrInfo, if (producesResults) classOf[AnyRef] else classOf[Boolean])
    }

    override protected def generateSuccess()(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        if (function.producesResults) {
            vis.visitInsn(Opcodes.ARETURN)
        } else {
            vis.visitInsn(Opcodes.ICONST_1)
            vis.visitInsn(Opcodes.IRETURN)
        }
    }

    override protected def generateFailure()(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        if (function.producesResults) {
            vis.loadObject(FailMarker)
            vis.visitInsn(Opcodes.ARETURN)
        } else {
            vis.visitInsn(Opcodes.ICONST_0)
            vis.visitInsn(Opcodes.IRETURN)
        }
    }

    override protected def jumpUsingReturnValue(pos: Int, instrInfo: InstrInfo, returnType: Class[?], intKind: JitImpl.IntKind)(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        if (function.tailInstrs(pos)) {
            if ((returnType eq classOf[Any]) && function.producesResults) {
                vis.visitInsn(Opcodes.ARETURN)
                return
            }
            if ((returnType eq classOf[Boolean]) && !function.producesResults && instrInfo.jumpPaths.isEmpty) {
                vis.visitInsn(Opcodes.IRETURN)
                return
            }
        }
        super.jumpUsingReturnValue(pos, instrInfo, returnType, intKind)
    }
}
