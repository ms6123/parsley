package parsley.internal.machine.jit.codegen

import scala.annotation.tailrec
import scala.collection.mutable

import parsley.internal.machine.jit.codegen.FunctionGenerator.Constants.*
import parsley.internal.machine.Context
import parsley.internal.machine.instructions.*
import parsley.internal.machine.instructions.JitImpl.Param
import parsley.internal.machine.jit.*

import org.objectweb.asm.{Label, Opcodes, Type}

private [codegen] abstract class FunctionGenerator(function: ParserFunction, classVisitor: ClassGenContext#ClassGenVisitor) {
    protected val implName: String
    protected val implDesc: String
    protected def implIsStatic: Boolean
    protected def contextIndex: Int

    private val baseLocalIndex = contextIndex + 1
    private val instrLabels = function.instrs.map(_ => new Label())
    private val failureLabel = new Label()

    def generate(): Unit = {
        implicit val vis = classVisitor.visitMethod(
            Opcodes.ACC_PUBLIC | (if (implIsStatic) Opcodes.ACC_STATIC else 0),
            implName,
            implDesc,
            null, null
        )
        generateImplStart()
        for ((instr, pos) <- function.instrs.zipWithIndex) {
            val instrInfo = function.info.instrInfos(pos)
            val fallThroughLabel = labelForPos(pos + 1)

            vis.visitLabel(instrLabels(pos))

            //                vis.loadObject(instr)
            //                vis.loadInt(pos)
            //                vis.visitVarInsn(Opcodes.ALOAD, 0)
            //                vis.visitMethodInsn(Opcodes.INVOKESTATIC, JIT_RUNTIME, "beforeInstruction", "(Ljava/lang/Object;ILjava/lang/Object;)V", false)

            instr match {
                case Halt | Return =>
                    generateSuccess()
                case Call(id, producesResults) =>
                    generateCall(pos, instrInfo, id, producesResults)
                case Push(x) =>
                    val successor = instrInfo.fallThroughPath.get

                    vis.loadObject(x.asInstanceOf[AnyRef])

                    jumpToSuccessor(successor, fallThroughLabel)
                case Fresh(x) =>
                    val successor = instrInfo.fallThroughPath.get

                    vis.loadObject(x)
                    vis.callMethod(Members.Functions.APPLY0)

                    jumpToSuccessor(successor, fallThroughLabel)
                case _: Case =>
                    val leftSuccessor = instrInfo.jumpPaths.head.successor
                    val rightSuccessor = instrInfo.fallThroughPath.get

                    val rightLabel = new Label()

                    vis.visitInsn(Opcodes.DUP)
                    vis.visitTypeInsn(Opcodes.INSTANCEOF, Type.getInternalName(classOf[Left[?, ?]]))
                    vis.visitJumpInsn(Opcodes.IFEQ, rightLabel)

                    vis.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(classOf[Left[?, ?]]))
                    vis.callMethod(Members.Either.LEFT_VALUE)
                    performAfterActions(leftSuccessor.afterActions)
                    vis.visitJumpInsn(Opcodes.GOTO, labelForPos(leftSuccessor.pc))

                    vis.visitLabel(rightLabel)
                    vis.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(classOf[Right[?, ?]]))
                    vis.callMethod(Members.Either.RIGHT_VALUE)

                    jumpToSuccessor(rightSuccessor, fallThroughLabel)
                case _: ManyUntil =>
                    val continueSuccessor = instrInfo.jumpPaths.head.successor
                    val stopSuccessor = instrInfo.fallThroughPath.get

                    val stopLabel = new Label()

                    vis.visitInsn(Opcodes.SWAP)
                    vis.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(classOf[mutable.Builder[?, ?]]))
                    vis.visitInsn(Opcodes.SWAP)

                    vis.visitInsn(Opcodes.DUP)
                    vis.loadObject(ManyUntil.Stop)
                    vis.visitJumpInsn(Opcodes.IF_ACMPEQ, stopLabel)

                    vis.callMethod(Members.Builder.ADD_ONE)
                    performAfterActions(continueSuccessor.afterActions)
                    vis.visitJumpInsn(Opcodes.GOTO, labelForPos(continueSuccessor.pc))

                    vis.visitLabel(stopLabel)
                    vis.visitInsn(Opcodes.POP)
                    vis.callMethod(Members.Builder.RESULT)

                    jumpToSuccessor(stopSuccessor, fallThroughLabel)
                case WhiteSpaceLike(impl) =>
                    vis.loadObject(impl)
                    loadContext()
                    vis.callMethod(Members.WhiteSpaceLikeImpl.APPLY)
                    jumpUsingReturnValue(pos, instrInfo, classOf[Boolean])
                case table: JumpTable =>
                    applyJumpTable(pos, instrInfo, table)
                case specialized: SpecializedInstr =>
                    applySpecialized(pos, instrInfo, specialized)
                case _ =>
                    vis.loadObject(instr)
                    loadContext()
                    vis.loadInt(pos)
                    vis.callMethod(Members.Instr.APPLY)
                    jumpUsingPc(pos, instrInfo)
            }
        }

        vis.visitLabel(failureLabel)
        generateFailure()

        vis.visitEnd()
    }

    protected def generateCall(pos: Int, instrInfo: InstrInfo, id: Int, producesResults: Boolean)(implicit vis: ClassGenContext#MethodGenVisitor): Unit

    protected def generateImplStart()(implicit vis: ClassGenContext#MethodGenVisitor): Unit = ()

    protected def generateSuccess()(implicit vis: ClassGenContext#MethodGenVisitor): Unit

    protected def generateFailure()(implicit vis: ClassGenContext#MethodGenVisitor): Unit

    protected def loadContext()(implicit vis: ClassGenContext#MethodGenVisitor): Unit =
        vis.visitVarInsn(Opcodes.ALOAD, contextIndex)

    protected def labelForPos(pos: Int) = if (pos == -1) failureLabel else instrLabels.applyOrElse(pos, (_: Int) => null)

    protected def handlerLocal(label: Int) = function.info.handlerSlots.get(label).map(_ + baseLocalIndex)

    protected def performAfterActions(actions: AfterActions)(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        CodeGenUtils.popN(actions.popOperands)

        if (actions.pushHandlers.nonEmpty) {
            loadContext()
            vis.callMethod(Members.Context.GET_OFFSET)
            CodeGenUtils.dupN(actions.pushHandlers.size - 1)
        }
        for (handler <- actions.pushHandlers) {
            vis.visitVarInsn(Opcodes.ISTORE, handlerLocal(handler).get)
        }
    }

    private def jumpToSuccessor(successor: Successor, fallThroughLabel: Label)(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        performAfterActions(successor.afterActions)
        CodeGenUtils.goToLabel(labelForPos(successor.pc), fallThroughLabel)
    }

    private def jumpUsingPc(pos: Int, instrInfo: InstrInfo)(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        CodeGenUtils.jumpDispatch(
            instrInfo.allPaths(pos).map(it => JumpPath(it.indicator, labelForPos(it.successor.pc), combineActions(it.successor.afterActions))),
            labelForPos(pos + 1)
        )
    }

    protected def combineActions(actions: AfterActions)(implicit vis: ClassGenContext#MethodGenVisitor) =
        if (actions.isEmpty) None else Some(() => performAfterActions(actions))

    protected def jumpUsingReturnValue(pos: Int, instrInfo: InstrInfo, returnType: Class[?], intKind: JitImpl.IntKind = JitImpl.IntKind.Pc)
                                      (implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        val fallThroughLabel = labelForPos(pos + 1)

        if (returnType eq classOf[Int]) {
            val boxingMethod = intKind match {
                case JitImpl.IntKind.Pc =>
                    jumpUsingPc(pos, instrInfo)
                    return
                case JitImpl.IntKind.Char =>
                    classOf[java.lang.Character].getMethod("valueOf", classOf[Char])
                case JitImpl.IntKind.CodePoint =>
                    classOf[java.lang.Integer].getMethod("valueOf", classOf[Int])
            }
            val Some(Successor(badPc, badAfterActions)) = instrInfo.badPath
            val Seq(IndicatedSuccessor(_, goodSucc)) = instrInfo.goodPaths(pos)

            val goodLabel = new Label()

            vis.visitInsn(Opcodes.DUP)
            vis.visitJumpInsn(Opcodes.IFGE, goodLabel)
            performAfterActions(badAfterActions)
            vis.visitJumpInsn(Opcodes.GOTO, labelForPos(badPc))

            vis.visitLabel(goodLabel)
            vis.callMethod(boxingMethod)

            jumpToSuccessor(goodSucc, fallThroughLabel)
            return
        }

        if (returnType eq classOf[Boolean]) {
            if (instrInfo.allPaths(pos).map(_.successor).toSet.size <= 1) {
                jumpUsingPc(pos, instrInfo)
                return
            }

            require(instrInfo.allPaths(pos).size <= 2 && instrInfo.fallThroughPath.nonEmpty)

            val Successor(falsePc, falseAfterActions) = instrInfo.badPath.getOrElse(instrInfo.jumpPaths.head.successor)
            val trueSucc = instrInfo.fallThroughPath.get
            if (falseAfterActions.isEmpty) {
                vis.visitJumpInsn(Opcodes.IFEQ, labelForPos(falsePc))
            } else {
                val goodLabel = new Label()
                vis.visitJumpInsn(Opcodes.IFNE, goodLabel)
                performAfterActions(falseAfterActions)
                vis.visitJumpInsn(Opcodes.GOTO, labelForPos(falsePc))
                vis.visitLabel(goodLabel)
            }

            jumpToSuccessor(trueSucc, fallThroughLabel)
            return
        }

        val goodPaths = instrInfo.goodPaths(pos).map(_.successor).toSet

        instrInfo.badPath match {
            case Some(badSucc@Successor(badPc, badAfterActions)) =>
                if (goodPaths.isEmpty || goodPaths == Set(badSucc)) {
                    jumpToSuccessor(badSucc, fallThroughLabel)
                    return
                }
                vis.visitInsn(Opcodes.DUP)
                vis.loadObject(FailMarker)
                if (badAfterActions.isEmpty) {
                    vis.visitJumpInsn(Opcodes.IF_ACMPEQ, labelForPos(badPc))
                } else {
                    val afterLabel = new Label()
                    vis.visitJumpInsn(Opcodes.IF_ACMPNE, afterLabel)
                    performAfterActions(badAfterActions)
                    vis.visitJumpInsn(Opcodes.GOTO, labelForPos(badPc))
                    vis.visitLabel(afterLabel)
                }
            case _ =>
        }

        val possiblePaths = returnType match {
            case t if t == classOf[Unit] =>
                goodPaths
            case t if t == classOf[Any] =>
                if (goodPaths.size <= 1) {
                    goodPaths
                } else {
                    val Successor(fallThroughPc, fallThroughActions) = instrInfo.fallThroughPath.get

                    if (fallThroughActions.isEmpty) {
                        vis.visitInsn(Opcodes.DUP)
                        vis.loadObject(FallthroughMarker)
                        vis.visitJumpInsn(Opcodes.IF_ACMPEQ, labelForPos(fallThroughPc))
                    } else {
                        val afterLabel = new Label()
                        vis.visitInsn(Opcodes.DUP)
                        vis.loadObject(FallthroughMarker)
                        vis.visitJumpInsn(Opcodes.IF_ACMPNE, afterLabel)

                        performAfterActions(fallThroughActions)

                        vis.visitJumpInsn(Opcodes.GOTO, labelForPos(fallThroughPc))
                        vis.visitLabel(afterLabel)
                    }

                    instrInfo.jumpPaths.map(_.successor).toSet
                }
        }

        require(possiblePaths.size <= 1)
        CodeGenUtils.jumpDispatch(possiblePaths.headOption.map(it => JumpPath(it.pc, labelForPos(it.pc), combineActions(it.afterActions))), fallThroughLabel)
    }

    private def performCustomActions(instrInfo: InstrInfo, actions: Array[JitImpl.Action])(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        for (action <- actions) {
            action match {
                case JitImpl.Action.PushTrue =>
                    vis.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/Boolean", "TRUE", "Ljava/lang/Boolean;")
                case JitImpl.Action.Swap =>
                    vis.visitInsn(Opcodes.SWAP)
                case JitImpl.Action.DupX1 =>
                    vis.visitInsn(Opcodes.DUP_X1)
                case JitImpl.Action.Dup =>
                    vis.visitInsn(Opcodes.DUP)
                case JitImpl.Action.UpdateCheckOffset =>
                    handlerLocal(instrInfo.stackInfo.handlers.head.pc).foreach { local =>
                        loadContext()
                        vis.callMethod(Members.Context.GET_OFFSET)
                        vis.visitVarInsn(Opcodes.ISTORE, local)
                    }
            }
        }
    }

    private def applySpecialized(pos: Int, instrInfo: InstrInfo, instr: Instr & SpecializedInstr)(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        val (method, info) = InstructionImpls.getImpl(instr)
        val trailingParams = method.getParameterTypes.view.drop(info.consumeOperands + info.constants.length).toArray

        performCustomActions(instrInfo, info.beforeActions)

        if (info.noop) {
            performAfterActions(AfterActions(popOperands = info.consumeOperands))
        } else {
            val instrClass = method.getDeclaringClass

            info.consumeOperands match {
                case 0 =>
                    vis.loadAny(instr, instrClass)
                case 1 =>
                    vis.loadAny(instr, instrClass)
                    vis.visitInsn(Opcodes.SWAP)
                case 2 =>
                    vis.loadAny(instr, instrClass)
                    vis.visitInsn(Opcodes.DUP_X2)
                    vis.visitInsn(Opcodes.POP)
                case 3 =>
                    val localOffset = freeLocalOffset(instrInfo)

                    val toStore = info.consumeOperands - 2
                    for (local <- toStore - 1 to 0 by -1) {
                        vis.visitVarInsn(Opcodes.ASTORE, local + localOffset)
                    }
                    vis.loadAny(instr, instrClass)
                    vis.visitInsn(Opcodes.DUP_X2)
                    vis.visitInsn(Opcodes.POP)
                    for (local <- 0 until toStore) {
                        vis.visitVarInsn(Opcodes.ALOAD, local + localOffset)
                    }
            }

            for (constantName <- info.constants) {
                val method = instrClass.getMethod(constantName)
                vis.loadAny(method.invoke(instr), method.getReturnType)
            }

            val paramInfos = info.params.iterator
            for (ty <- trailingParams) {
                if (ty eq classOf[Context]) {
                    loadContext()
                } else {
                    paramInfos.next() match {
                        case Param.Pc => vis.loadInt(pos)
                        case Param.HandlerCheck => vis.visitVarInsn(Opcodes.ILOAD, handlerLocal(instrInfo.stackInfo.handlers.head.pc).get)
                    }
                }
            }

            vis.callMethod(method)
        }

        performCustomActions(instrInfo, info.afterActions)

        jumpUsingReturnValue(pos, instrInfo, method.getReturnType, info.intReturnKind)
    }

    private def applyJumpTable(pos: Int, instrInfo: InstrInfo, instr: JumpTable)(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        val destinationsByIndicator = instrInfo.jumpPaths.map(it => it.indicator -> it.successor).toMap
        val defaultSuccessor = destinationsByIndicator(instr.defaultIndicator)
        val charLocal = freeLocalOffset(instrInfo)
        val defaultLabel = new Label()

        @tailrec def dispatch(jumpTable: JumpTablePreds, loadChar: Boolean = false)(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
            val fallThroughLabel = if (jumpTable.next eq null) defaultLabel else new Label()

            if (loadChar) {
                vis.visitVarInsn(Opcodes.ILOAD, charLocal)
            }

            jumpTable match {
                case charMap: JumpTableCharMapPred =>
                    val jumpPaths = charMap.map.view.map { case (char, (indicator, _)) =>
                        val dest = destinationsByIndicator(indicator)
                        JumpPath(char, labelForPos(dest.pc), combineActions(dest.afterActions))
                    }.toSeq

                    CodeGenUtils.switchDispatch(jumpPaths, fallThroughLabel, None)
                case charFun: JumpTableCharFunPred =>
                    val dest = destinationsByIndicator(charFun.label)

                    vis.callMethod(Members.Boxing.BOX_TO_CHARACTER)
                    vis.loadObject(charFun.pred)
                    vis.visitInsn(Opcodes.SWAP)
                    vis.callMethod(Members.Functions.APPLY1)
                    vis.callMethod(Members.Boxing.UNBOX_TO_BOOLEAN)
                    vis.visitJumpInsn(Opcodes.IFEQ, fallThroughLabel)
                    performAfterActions(dest.afterActions)
                    vis.visitJumpInsn(Opcodes.GOTO, labelForPos(dest.pc))
            }

            vis.visitLabel(fallThroughLabel)
            if (jumpTable.next ne null) {
                dispatch(jumpTable.next, loadChar = true)
            } else {
                performAfterActions(defaultSuccessor.afterActions)
                CodeGenUtils.goToLabel(labelForPos(defaultSuccessor.pc), labelForPos(pos + 1))
            }
        }

        loadContext()
        vis.callMethod(Members.Context.MORE_INPUT)
        vis.visitJumpInsn(Opcodes.IFEQ, defaultLabel)

        loadContext()
        vis.callMethod(Members.Context.PEEK_CHAR)
        if (instr.jumpTable.next ne null) {
            vis.visitInsn(Opcodes.DUP)
            vis.visitVarInsn(Opcodes.ISTORE, charLocal)
        }

        dispatch(instr.jumpTable)
    }

    protected def freeLocalOffset(instrInfo: InstrInfo): Int =
        instrInfo.stackInfo.handlers.view.map(_.pc).map(handlerLocal).collectFirst { case Some(local) => local }.getOrElse(0) + baseLocalIndex
}

private [codegen] object FunctionGenerator {
    object Constants {
        val JIT_CONTEXT: Type = Type.getType(classOf[JitContext])
        val CONTEXT: Type = Type.getType(classOf[Context])
        val IMPL_NAME = "parse"
        private [FunctionGenerator] val IMPL_DESC: String = Type.getMethodDescriptor(Type.getType(classOf[AnyRef]), JIT_CONTEXT)
        private [FunctionGenerator] val VOID_IMPL_DESC: String = Type.getMethodDescriptor(Type.getType(classOf[Boolean]), JIT_CONTEXT)
    }

    def implDesc(producesResults: Boolean): String = if (producesResults) IMPL_DESC else VOID_IMPL_DESC
}
