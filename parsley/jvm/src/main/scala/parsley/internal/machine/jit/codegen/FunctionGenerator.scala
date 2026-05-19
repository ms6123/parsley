package parsley.internal.machine.jit.codegen

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

    private val baseLocalIndex = if (implIsStatic) 1 else 2
    private val instrLabels = function.instrs.map(_ => new Label())
    private val successLabel = new Label()
    private val failureLabel = new Label()

    def generate(): Unit = {
        implicit val vis = classVisitor.visitMethod(
            Opcodes.ACC_PUBLIC | (if (implIsStatic) Opcodes.ACC_STATIC else 0),
            implName,
            implDesc,
            null, null
        )
        generateImplStart()
        for ((instr, pos) <- function.instrs.view.zipWithIndex; instrInfo <- function.info.instrInfos(pos)) {
            vis.visitLabel(instrLabels(pos))

            //                vis.loadObject(instr)
            //                vis.loadInt(pos)
            //                vis.visitVarInsn(Opcodes.ALOAD, 0)
            //                vis.visitMethodInsn(Opcodes.INVOKESTATIC, JIT_RUNTIME, "beforeInstruction", "(Ljava/lang/Object;ILjava/lang/Object;)V", false)

            instr match {
                case Halt | Return =>
                    vis.visitJumpInsn(Opcodes.GOTO, successLabel)
                case Call(id, producesResults) =>
                    generateCall(pos, instrInfo, id, producesResults)
                case Push(x) =>
                    vis.loadObject(x.asInstanceOf[AnyRef])
                case Fresh(x) =>
                    vis.loadObject(x)
                    vis.callMethod(Members.Functions.APPLY0)
                case Case(label) =>
                    val rightLabel = new Label()

                    vis.visitInsn(Opcodes.DUP)
                    vis.visitTypeInsn(Opcodes.INSTANCEOF, Type.getInternalName(classOf[Left[?, ?]]))
                    vis.visitJumpInsn(Opcodes.IFEQ, rightLabel)

                    vis.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(classOf[Left[?, ?]]))
                    vis.callMethod(Members.Either.LEFT_VALUE)
                    vis.visitJumpInsn(Opcodes.GOTO, labelForPos(label))

                    vis.visitLabel(rightLabel)
                    vis.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(classOf[Right[?, ?]]))
                    vis.callMethod(Members.Either.RIGHT_VALUE)
                case ManyUntil(label) =>
                    val stopLabel = new Label()

                    vis.visitInsn(Opcodes.SWAP)
                    vis.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(classOf[mutable.Builder[?, ?]]))
                    vis.visitInsn(Opcodes.SWAP)

                    vis.visitInsn(Opcodes.DUP)
                    vis.loadObject(ManyUntil.Stop)
                    vis.visitJumpInsn(Opcodes.IF_ACMPEQ, stopLabel)

                    vis.callMethod(Members.Builder.ADD_ONE)
                    vis.visitJumpInsn(Opcodes.GOTO, labelForPos(label))

                    vis.visitLabel(stopLabel)
                    vis.visitInsn(Opcodes.POP)
                    vis.callMethod(Members.Builder.RESULT)
                case WhiteSpaceLike(impl) =>
                    vis.loadObject(impl)
                    loadContext()
                    vis.callMethod(Members.WhiteSpaceLikeImpl.APPLY)
                    jumpUsingReturnValue(pos, instrInfo, classOf[Boolean])
                case specialized: SpecializedInstr =>
                    applySpecialized(pos, instrInfo, specialized)
                case _ =>
                    vis.loadObject(instr)
                    loadContext()
                    vis.loadInt(pos)
                    vis.callMethod(Members.Instr.APPLY)
                    performAfterActions(instrInfo.afterActions)
                    jumpUsingPc(pos, instrInfo)
            }
        }

        vis.visitLabel(successLabel)
        generateSuccess()
        vis.visitLabel(failureLabel)
        generateFailure()

        vis.visitEnd()
    }

    protected def generateCall(pos: Int, instrInfo: InstrInfo, id: Int, producesResults: Boolean)(implicit vis: ClassGenContext#MethodGenVisitor): Unit

    protected def generateImplStart()(implicit vis: ClassGenContext#MethodGenVisitor): Unit = ()

    protected def generateSuccess()(implicit vis: ClassGenContext#MethodGenVisitor): Unit

    protected def generateFailure()(implicit vis: ClassGenContext#MethodGenVisitor): Unit

    protected def loadContext()(implicit vis: ClassGenContext#MethodGenVisitor): Unit =
        vis.visitVarInsn(Opcodes.ALOAD, if (implIsStatic) 0 else 1)

    protected def labelForPos(pos: Int) = if (pos == -1) failureLabel else instrLabels.applyOrElse(pos, (_: Int) => null)

    protected def handlerLocal(label: Int) = function.info.handlerSlots.get(label).map(_ + baseLocalIndex)

    protected def performAfterActions(actions: Seq[AfterAction])(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        for (action <- actions) {
            action match {
                case AfterAction.PopOperands(n) =>
                    for (_ <- 1 to n / 2) {
                        vis.visitInsn(Opcodes.POP2)
                    }
                    if (n % 2 == 1) {
                        vis.visitInsn(Opcodes.POP)
                    }
                case AfterAction.PushHandler(label) =>
                    handlerLocal(label).foreach { local =>
                        loadContext()
                        vis.callMethod(Members.Context.GET_OFFSET)
                        vis.visitVarInsn(Opcodes.ISTORE, local)
                    }
            }
        }
    }

    private def jumpUsingPc(pos: Int, instrInfo: InstrInfo)(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        val successorPaths = Seq.newBuilder[JumpPath]

        successorPaths ++= instrInfo.goodPaths.map(it => JumpPath(it.pc, labelForPos(it.pc), combineActions(it.afterActions)))
        successorPaths ++= instrInfo.badPath.map(it => JumpPath(-1, labelForPos(it.pc), combineActions(it.afterActions)))

        CodeGenUtils.jumpDispatch(successorPaths.result(), labelForPos(pos + 1))
    }

    private def combineActions(actions: Seq[AfterAction])(implicit vis: ClassGenContext#MethodGenVisitor) = actions match {
        case Seq() => None
        case actions => Some(() => performAfterActions(actions))
    }

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
            val Seq(Successor(goodPc, goodAfterActions)) = instrInfo.goodPaths

            val goodLabel = new Label()

            vis.visitInsn(Opcodes.DUP)
            vis.visitJumpInsn(Opcodes.IFGE, goodLabel)
            performAfterActions(badAfterActions)
            vis.visitJumpInsn(Opcodes.GOTO, labelForPos(badPc))

            vis.visitLabel(goodLabel)
            vis.callMethod(boxingMethod)

            performAfterActions(goodAfterActions)
            CodeGenUtils.goToLabel(labelForPos(goodPc), fallThroughLabel)
            return
        }

        if (returnType eq classOf[Boolean]) {
            if (instrInfo.allPaths.size <= 1) {
                jumpUsingPc(pos, instrInfo)
                return
            }

            require(instrInfo.allPaths.size <= 2 && instrInfo.fallThroughPath.nonEmpty)

            val Successor(falsePc, falseAfterActions) = instrInfo.badPath.getOrElse(instrInfo.jumpPaths.head)
            val Successor(truePc, trueAfterActions) = instrInfo.fallThroughPath.get
            if (falseAfterActions.isEmpty) {
                vis.visitJumpInsn(Opcodes.IFEQ, labelForPos(falsePc))
            } else {
                val goodLabel = new Label()
                vis.visitJumpInsn(Opcodes.IFNE, goodLabel)
                performAfterActions(falseAfterActions)
                vis.visitJumpInsn(Opcodes.GOTO, labelForPos(falsePc))
                vis.visitLabel(goodLabel)
            }

            performAfterActions(trueAfterActions)
            CodeGenUtils.goToLabel(labelForPos(truePc), fallThroughLabel)
            return
        }

        val goodPaths = instrInfo.goodPaths

        instrInfo.badPath match {
            case Some(Successor(badPc, badAfterActions)) =>
                if (goodPaths.isEmpty) {
                    performAfterActions(badAfterActions)
                    CodeGenUtils.goToLabel(labelForPos(badPc), fallThroughLabel)
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

                    instrInfo.jumpPaths
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
            performAfterActions(Seq(AfterAction.PopOperands(info.consumeOperands)))
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
                    val currentHandlers = instrInfo.stackInfo.handlers.view.map(_.pc)
                    val localOffset = currentHandlers.map(handlerLocal).collectFirst { case Some(local) => local }.getOrElse(0) + baseLocalIndex

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

        performAfterActions(instrInfo.afterActions)

        jumpUsingReturnValue(pos, instrInfo, method.getReturnType, info.intReturnKind)
    }
}

private [codegen] object FunctionGenerator {
    object Constants {
        val JIT_CONTEXT: Type = Type.getType(classOf[JitContext])
        val CONTEXT: Type = Type.getType(classOf[Context])
        val IMPL_NAME = "parse"
        val IMPL_DESC: String = Type.getMethodDescriptor(Type.getType(classOf[AnyRef]), JIT_CONTEXT)
        val VOID_IMPL_DESC: String = Type.getMethodDescriptor(Type.getType(classOf[Boolean]), JIT_CONTEXT)
    }

    def implDesc(producesResults: Boolean): String = if (producesResults) IMPL_DESC else VOID_IMPL_DESC
}
