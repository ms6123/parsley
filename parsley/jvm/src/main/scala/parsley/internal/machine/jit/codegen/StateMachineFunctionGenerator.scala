package parsley.internal.machine.jit.codegen

import parsley.internal.machine.instructions.FailMarker
import parsley.internal.machine.jit.*
import parsley.internal.machine.jit.codegen.FunctionGenerator.Constants.IMPL_NAME
import parsley.internal.machine.jit.codegen.StateMachineFunctionGenerator.Constants
import parsley.internal.machine.ParseRunner

import org.objectweb.asm.{Label, Opcodes, Type}

class StateMachineFunctionGenerator(function: ParserFunction, ctx: ParserGenerator, classVisitor: ClassGenContext#ClassGenVisitor)
    extends FunctionGenerator(function, classVisitor) {
    private val self = Type.getObjectType(ParserGenerator.className(function.id))
    private val returnLabels = function.suspensionPoints.map(it => it -> new Label()).toMap
    private val isPassthrough = function.isPassthrough
    private val endLabel = new Label()

    override protected val implName: String = if (isPassthrough) Constants.START_NAME else Constants.IMPL_NAME
    override protected val implDesc: String = if (isPassthrough) Constants.START_DESC else Constants.IMPL_DESC

    override protected def implIsStatic: Boolean = isPassthrough
    override protected def contextIndex: Int = 1

    override def generate(): Unit = {
        if (!isPassthrough) {
            generateFields()
        }
        generateEntrypoint()
        if (!isPassthrough) {
            generateStart()
        }
        super.generate()
        if (!isPassthrough) {
            generateCtor()
        }
    }

    override protected def generateImplStart()(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        if (isPassthrough) {
            return
        }
        vis.visitVarInsn(Opcodes.ALOAD, 0)
        vis.visitFieldInsn(Opcodes.GETFIELD, self.getInternalName, Constants.LABEL_NAME, Constants.LABEL_DESC)
        CodeGenUtils.jumpDispatch(
            JumpPath(0, labelForPos(0), None) +:
                returnLabels.toSeq.map { case (pos, label) => JumpPath(pos + 1, label, None) },
            labelForPos(0)
        )
    }

    override protected def generateCall(pos: Int, instrInfo: InstrInfo, id: Int, producesResults: Boolean)(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        val calleeType = Type.getObjectType(ParserGenerator.className(id))

        val shouldSuspend = returnLabels.contains(pos)

        if (!shouldSuspend && ctx.resolveCall(id).isCyclic) {
            // Tail call
            loadNextContinuation()
            loadContext()
            vis.visitMethodInsn(Opcodes.INVOKESTATIC, calleeType.getInternalName, Constants.START_NAME, Constants.START_DESC, false)
            vis.visitInsn(Opcodes.ARETURN)
            return
        }

        if (shouldSuspend) {
            generateSuspension(pos, instrInfo, producesResults) {
                vis.visitVarInsn(Opcodes.ALOAD, 0)
                loadContext()
                vis.visitMethodInsn(Opcodes.INVOKESTATIC, calleeType.getInternalName, Constants.START_NAME, Constants.START_DESC, false)
            }
        } else {
            // Regular call
            val isTail = function.tailInstrs(pos)
            loadContext()
            vis.visitMethodInsn(Opcodes.INVOKESTATIC, calleeType.getInternalName, IMPL_NAME, FunctionGenerator.implDesc(producesResults), false)
            if (isTail && producesResults && function.producesResults) {
                loadNextContinuation()
                vis.visitInsn(Opcodes.DUP_X1)
                vis.visitInsn(Opcodes.SWAP)
                vis.putField(Members.Continuation.RESULT)
                vis.visitInsn(Opcodes.ARETURN)
            } else if (isTail && !producesResults && !function.producesResults) {
                loadNextContinuation()
                vis.visitInsn(Opcodes.DUP_X1)
                vis.visitInsn(Opcodes.SWAP)
                vis.callMethod(Members.Boxing.BOX_TO_BOOLEAN)
                vis.putField(Members.Continuation.RESULT)
                vis.visitInsn(Opcodes.ARETURN)
            } else {
                jumpUsingReturnValue(pos, instrInfo, if (producesResults) classOf[AnyRef] else classOf[Boolean])
            }
        }
    }

    override protected def generateDynCall(pos: Int, instrInfo: InstrInfo, f: (Any, Int, Boolean) => ParseRunner)
                                          (implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        // We need to save/restore 1 fewer operands than we have on the stack, since the topmost one we will use for the DynCall
        val paramLocal = freeLocalOffset(instrInfo)
        vis.visitVarInsn(Opcodes.ASTORE, paramLocal)

        val fakeInfo = instrInfo.copy(stackInfo = instrInfo.stackInfo.copy(stacksz = instrInfo.stackInfo.stacksz - 1))
        generateSuspension(pos, fakeInfo, true) {
            vis.visitVarInsn(Opcodes.ALOAD, paramLocal)
            vis.visitVarInsn(Opcodes.ALOAD, 0)
            loadContext()
            vis.loadObject(f)
            vis.callMethod(Members.JitRuntime.DYN_CALL)
        }
    }

    override protected def generateSuccess()(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        if (!function.producesResults) {
            vis.getField(Members.Boolean.TRUE)
        }
        vis.visitJumpInsn(Opcodes.GOTO, endLabel)
    }

    override protected def generateFailure()(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        if (function.producesResults) {
            vis.loadObject(FailMarker)
        } else {
            vis.getField(Members.Boolean.FALSE)
        }
        vis.visitLabel(endLabel)

        vis.visitVarInsn(Opcodes.ALOAD, 0)
        vis.visitInsn(Opcodes.SWAP)

        if (!isPassthrough) {
            vis.callMethod(Members.Continuation.RETURN_WITH)
        } else {
            vis.putField(Members.Continuation.RESULT)
            vis.visitVarInsn(Opcodes.ALOAD, 0)
        }
        vis.visitInsn(Opcodes.ARETURN)
    }

    private def generateSuspension(pos: Int, instrInfo: InstrInfo, producesResults: Boolean)(createContinuation: =>Unit)
                                  (implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        val returnLabel = returnLabels(pos)

        for (i <- instrInfo.stackInfo.stacksz - 1 to 0 by -1) {
            vis.visitVarInsn(Opcodes.ALOAD, 0)
            vis.visitInsn(Opcodes.SWAP)
            vis.visitFieldInsn(Opcodes.PUTFIELD, self.getInternalName, Constants.savedStackName(i), Constants.SAVED_STACK_DESC)
        }

        for (handler <- instrInfo.stackInfo.handlers) {
            function.info.handlerSlots.get(handler.pc) match {
                case Some(slot) =>
                    vis.visitVarInsn(Opcodes.ALOAD, 0)
                    vis.visitVarInsn(Opcodes.ILOAD, handlerLocal(handler.pc).get)
                    vis.visitFieldInsn(Opcodes.PUTFIELD, self.getInternalName, Constants.savedCheckName(slot), Constants.SAVED_CHECK_DESC)
                case None =>
            }
        }

        vis.visitVarInsn(Opcodes.ALOAD, 0)
        vis.loadInt(pos + 1)
        vis.visitFieldInsn(Opcodes.PUTFIELD, self.getInternalName, Constants.LABEL_NAME, Constants.LABEL_DESC)

        createContinuation

        vis.visitInsn(Opcodes.ARETURN)

        vis.visitLabel(returnLabel)

        for (handler <- instrInfo.stackInfo.handlers) {
            function.info.handlerSlots.get(handler.pc) match {
                case Some(slot) =>
                    vis.visitVarInsn(Opcodes.ALOAD, 0)
                    vis.visitFieldInsn(Opcodes.GETFIELD, self.getInternalName, Constants.savedCheckName(slot), Constants.SAVED_CHECK_DESC)
                    vis.visitVarInsn(Opcodes.ISTORE, handlerLocal(handler.pc).get)
                case None =>
            }
        }

        for (i <- 0 until instrInfo.stackInfo.stacksz) {
            vis.visitVarInsn(Opcodes.ALOAD, 0)
            vis.visitFieldInsn(Opcodes.GETFIELD, self.getInternalName, Constants.savedStackName(i), Constants.SAVED_STACK_DESC)
        }

        vis.visitVarInsn(Opcodes.ALOAD, 0)
        vis.getField(Members.Continuation.RESULT)
        if (!producesResults) {
            vis.callMethod(Members.Boxing.UNBOX_TO_BOOLEAN)
        }
        jumpUsingReturnValue(pos, instrInfo, if (producesResults) classOf[AnyRef] else classOf[Boolean])
    }

    private def generateFields(): Unit = {
        classVisitor.visitField(Opcodes.ACC_PRIVATE, Constants.LABEL_NAME, Constants.LABEL_DESC, null, null).visitEnd()

        val callSaveInfos = returnLabels.keys.view.map(function.info.instrInfos(_)).map(_.stackInfo)
        val maxSavedStack = callSaveInfos.map(_.stacksz).foldLeft(0)(_ max _)
        val maxSavedChecks = callSaveInfos.flatMap(_.handlers).map(_.pc).flatMap(function.info.handlerSlots.get).foldLeft(-1)(_ max _) + 1

        for (i <- 0 until maxSavedStack) {
            classVisitor.visitField(Opcodes.ACC_PRIVATE, Constants.savedStackName(i), Constants.SAVED_STACK_DESC, null, null).visitEnd()
        }
        for (i <- 0 until maxSavedChecks) {
            classVisitor.visitField(Opcodes.ACC_PRIVATE, Constants.savedCheckName(i), Constants.SAVED_CHECK_DESC, null, null).visitEnd()
        }
    }

    private def generateEntrypoint(): Unit = {
        val vis = classVisitor.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, FunctionGenerator.Constants.IMPL_NAME, FunctionGenerator.implDesc(function.producesResults), null, null)

        vis.visitVarInsn(Opcodes.ALOAD, 0)
        vis.callMethod(Members.Context.GET_RESULT_HOLDER)

        vis.visitVarInsn(Opcodes.ALOAD, 0)
        vis.visitMethodInsn(Opcodes.INVOKESTATIC, self.getInternalName, Constants.START_NAME, Constants.START_DESC, false)

        vis.visitVarInsn(Opcodes.ALOAD, 0)
        vis.callMethod(Members.Continuation.RUN)

        if (function.producesResults) {
            vis.visitInsn(Opcodes.ARETURN)
        } else {
            vis.callMethod(Members.Boxing.UNBOX_TO_BOOLEAN)
            vis.visitInsn(Opcodes.IRETURN)
        }

        vis.visitEnd()
    }

    private def generateStart(): Unit = {
        val vis = classVisitor.visitMethod(Opcodes.ACC_STATIC, Constants.START_NAME, Constants.START_DESC, null, null)

        vis.visitTypeInsn(Opcodes.NEW, self.getInternalName)
        vis.visitInsn(Opcodes.DUP)

        vis.visitVarInsn(Opcodes.ALOAD, 0)

        vis.visitMethodInsn(Opcodes.INVOKESPECIAL, self.getInternalName, Constants.CTOR_NAME, Constants.CTOR_DESC, false)

        vis.visitInsn(Opcodes.ARETURN)
        vis.visitEnd()
    }

    private def generateCtor(): Unit = {
        val vis = classVisitor.visitMethod(Opcodes.ACC_PRIVATE, Constants.CTOR_NAME, Constants.CTOR_DESC, null, null)

        vis.visitVarInsn(Opcodes.ALOAD, 0)
        vis.visitVarInsn(Opcodes.ALOAD, 1)
        vis.visitMethodInsn(Opcodes.INVOKESPECIAL, Constants.CONTINUATION.getInternalName, Constants.CTOR_NAME, Constants.CTOR_DESC, false)

        vis.visitInsn(Opcodes.RETURN)
        vis.visitEnd()
    }

    private def loadNextContinuation()(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        vis.visitVarInsn(Opcodes.ALOAD, 0)
        if (!isPassthrough) {
            vis.getField(Members.Continuation.NEXT)
        }
    }
}

private object StateMachineFunctionGenerator {
    object Constants {
        val CONTINUATION: Type = Type.getType(classOf[Continuation])
        val START_NAME: String = "start"
        val START_DESC: String = Type.getMethodDescriptor(CONTINUATION, CONTINUATION, Type.getType(classOf[JitContext]))
        val CTOR_NAME: String = "<init>"
        val CTOR_DESC: String = Type.getMethodDescriptor(Type.VOID_TYPE, CONTINUATION)
        val IMPL_NAME: String = "step"
        val IMPL_DESC: String = Type.getMethodDescriptor(CONTINUATION, Type.getType(classOf[JitContext]))
        val LABEL_NAME: String = "label"
        val LABEL_DESC: String = "I"
        val SAVED_STACK_DESC: String = Type.getDescriptor(classOf[AnyRef])
        val SAVED_CHECK_DESC: String = Type.getDescriptor(classOf[Int])

        def savedStackName(i: Int): String = "stack" + i

        def savedCheckName(i: Int): String = "check" + i
    }
}
