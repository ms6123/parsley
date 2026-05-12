package parsley.internal.machine.jit

import java.lang.invoke.{MethodHandle, MethodHandles, MethodType}
import java.lang.reflect.Method

import scala.collection.mutable

import parsley.internal.machine.Context
import parsley.internal.machine.instructions.*

import org.objectweb.asm.{Label, Opcodes, Type}

private val JIT_CONTEXT = Type.getType(classOf[JitContext])
private val CONTEXT = Type.getType(classOf[Context])
private val IMPL_NAME = "parse"
private val IMPL_DESC = Type.getMethodDescriptor(Type.getType(classOf[AnyRef]), JIT_CONTEXT)
private val VOID_IMPL_DESC = Type.getMethodDescriptor(Type.getType(classOf[Boolean]), JIT_CONTEXT)

private object Methods {
    object Context {
        val IS_GOOD: Method = classOf[JitContext].getMethod("good")
        val GET_OFFSET: Method = classOf[JitContext].getMethod("offset")
    }

    object Instr {
        val APPLY: Method = classOf[Instr].getMethod("apply", classOf[Context], classOf[Int])
    }

    object Either {
        val LEFT_VALUE: Method = classOf[Left[?, ?]].getMethod("value")
        val RIGHT_VALUE: Method = classOf[Right[?, ?]].getMethod("value")
    }

    object Builder {
        val RESULT: Method = classOf[mutable.Builder[?, ?]].getMethod("result")
        val ADD_ONE: Method = classOf[mutable.Builder[?, ?]].getMethod("addOne", classOf[Any])
    }
}

private[jit] class ParserGenerator(private val functions: Array[ParserFunction]) {
    private val ctx = ClassGenContext()
    private val functionsById = functions.view.map(it => it.id -> it).toMap

    def generate(): MethodHandle = {
        val classes = functions.map(generate)
        MethodHandles.lookup().findStatic(classes.head, IMPL_NAME, MethodType.methodType(classOf[AnyRef], classOf[JitContext]))
    }

    private def generate(function: ParserFunction): Class[?] =
        ctx.newClass(Opcodes.ACC_PUBLIC, className(function.id)) { classVisitor =>
            val vis = classVisitor.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, IMPL_NAME, implDesc(function.producesResults), null, null)
            val instrLabels = function.instrs.map(_ => Label())
            val successLabel = Label()
            val failureLabel = Label()

            def loadContext(): Unit = vis.visitVarInsn(Opcodes.ALOAD, 0)

            def labelForPos(pos: Int) = if (pos == -1) failureLabel else instrLabels(pos)

            def handlerLocal(label: Int) = function.info.handlerSlots.get(label).map(_ + 1)

            def performActions(actions: Seq[AfterAction]): Unit = {
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
                                vis.callMethod(Methods.Context.GET_OFFSET)
                                vis.visitVarInsn(Opcodes.ISTORE, local)
                            }
                    }
                }
            }

            def jumpToSuccessors(pos: Int, successors: Seq[(Successor, Label)]): Unit = {
                val fallThroughLabel = instrLabels.applyOrElse(pos + 1, _ => null)

                successors match {
                    case Seq() =>
                        vis.visitJumpInsn(Opcodes.GOTO, successLabel)
                    case Seq((Successor(successorPc, afterActions), nextLabel)) if nextLabel eq fallThroughLabel =>
                        performActions(afterActions)
                    case Seq((Successor(successorPc, afterActions), nextLabel)) =>
                        performActions(afterActions)
                        vis.visitJumpInsn(Opcodes.GOTO, nextLabel)
                    case _ =>
                        successors match {
                            case Seq(path1, path2) if (path1._2 eq fallThroughLabel) || (path2._2 eq fallThroughLabel) =>
                                val ((Successor(fallThroughPc, fallThroughActions), _), (Successor(jumpPc, jumpActions), jumpLabel)) =
                                    if (path1._2 eq fallThroughLabel) (path1, path2) else (path2, path1)

                                vis.loadInt(fallThroughPc)
                                if (jumpActions.isEmpty) {
                                    vis.visitJumpInsn(Opcodes.IF_ICMPNE, jumpLabel)
                                } else {
                                    val fallThroughLabel = Label()
                                    vis.visitJumpInsn(Opcodes.IF_ICMPEQ, fallThroughLabel)
                                    performActions(jumpActions)
                                    vis.visitJumpInsn(Opcodes.GOTO, jumpLabel)
                                    vis.visitLabel(fallThroughLabel)
                                }
                                performActions(fallThroughActions)
                            case Seq((successor1, label1), (successor2, label2)) =>
                                vis.loadInt(successor1.pc)

                                (successor1.afterActions, successor2.afterActions) match {
                                    case (Seq(), Seq()) =>
                                        vis.visitJumpInsn(Opcodes.IF_ICMPEQ, label1)
                                        vis.visitJumpInsn(Opcodes.GOTO, label2)
                                    case (afterActions, Seq()) =>
                                        vis.visitJumpInsn(Opcodes.IF_ICMPNE, label2)
                                        performActions(afterActions)
                                        vis.visitJumpInsn(Opcodes.GOTO, label1)
                                    case (Seq(), afterActions) =>
                                        vis.visitJumpInsn(Opcodes.IF_ICMPEQ, label1)
                                        performActions(afterActions)
                                        vis.visitJumpInsn(Opcodes.GOTO, label2)
                                    case (afterActions1, afterActions2) =>
                                        val successor1Label = Label()
                                        vis.visitJumpInsn(Opcodes.IF_ICMPEQ, successor1Label)
                                        performActions(afterActions2)
                                        vis.visitJumpInsn(Opcodes.GOTO, label2)
                                        vis.visitLabel(successor1Label)
                                        performActions(afterActions1)
                                        vis.visitJumpInsn(Opcodes.GOTO, label1)
                                }
                            case _ =>
                                val keys = successors.toArray
                                keys.sortInPlaceBy(_._1.pc)
                                val afterSwitchTasks = mutable.Buffer.empty[() => Unit]

                                val switchLabels = keys.map {
                                    case (Successor(pc, Seq()), label) => label
                                    case (Successor(pc, afterActions), label) =>
                                        val newLabel = Label()
                                        afterSwitchTasks += (() => {
                                            vis.visitLabel(newLabel)
                                            performActions(afterActions)
                                            vis.visitJumpInsn(Opcodes.GOTO, label)
                                        })
                                        newLabel
                                }

                                vis.visitLookupSwitchInsn(switchLabels.last, keys.view.init.map(_._1.pc).toArray, switchLabels.init)

                                for (task <- afterSwitchTasks) {
                                    task()
                                }
                        }
                }
            }

            for ((instr, pos) <- function.instrs.view.zipWithIndex; instrInfo <- function.info.instrInfos(pos)) {
                vis.visitLabel(instrLabels(pos))

//                vis.loadObject(instr)
//                vis.loadInt(pos)
//                vis.visitVarInsn(Opcodes.ALOAD, 0)
//                vis.visitMethodInsn(Opcodes.INVOKESTATIC, JIT_RUNTIME, "beforeInstruction", "(Ljava/lang/Object;ILjava/lang/Object;)V", false)

                def jumpToAllSuccessors(): Unit = {
                    val successorLabels = Seq.newBuilder[(Successor, Label)]

                    successorLabels ++= instrInfo.goodPaths.map(it => it -> labelForPos(it.pc))
                    successorLabels ++= instrInfo.badPath.map(it => Successor(-1, it.afterActions) -> labelForPos(it.pc))

                    val result = successorLabels.result()
                    if (result.sizeIs <= 1) {
                        // No need for pc
                        vis.visitInsn(Opcodes.POP)
                    }
                    jumpToSuccessors(pos, successorLabels.result())
                }

                def jumpUsingReturnValue(returnType: Class[?]): Unit = {
                    if (returnType eq classOf[Int]) {
                        jumpToAllSuccessors()
                        return
                    }

                    if (returnType eq classOf[Boolean]) {
                        require(instrInfo.allPaths.sizeIs <= 2 && (instrInfo.jumpPaths.isEmpty || instrInfo.fallThroughPath.isEmpty))

                        if (instrInfo.allPaths.sizeIs <= 1) {
                            vis.visitInsn(Opcodes.POP)
                            jumpToSuccessors(pos, instrInfo.allPaths.map(it => it -> labelForPos(it.pc)))
                            return
                        }

                        val Successor(badPc, badAfterActions) = instrInfo.badPath.get
                        val Seq(Successor(goodPc, goodAfterActions)) = instrInfo.goodPaths
                        if (badAfterActions.isEmpty) {
                            vis.visitJumpInsn(Opcodes.IFEQ, labelForPos(badPc))
                        } else {
                            val goodLabel = Label()
                            vis.visitJumpInsn(Opcodes.IFNE, goodLabel)
                            performActions(badAfterActions)
                            vis.visitJumpInsn(Opcodes.GOTO, labelForPos(badPc))
                            vis.visitLabel(goodLabel)
                        }

                        jumpToSuccessors(pos, Seq(Successor(goodPc, goodAfterActions) -> labelForPos(goodPc)))
                        return
                    }

                    val goodPaths = instrInfo.goodPaths

                    instrInfo.badPath match {
                        case Some(Successor(badPc, badAfterActions)) =>
                            if (goodPaths.isEmpty) {
                                jumpToSuccessors(pos, Seq(Successor(badPc, badAfterActions) -> labelForPos(badPc)))
                                return
                            }
                            vis.visitInsn(Opcodes.DUP)
                            vis.loadObject(FailMarker)
                            if (badAfterActions.isEmpty) {
                                vis.visitJumpInsn(Opcodes.IF_ACMPEQ, labelForPos(badPc))
                            } else {
                                val afterLabel = Label()
                                vis.visitJumpInsn(Opcodes.IF_ACMPNE, afterLabel)
                                performActions(badAfterActions)
                                vis.visitJumpInsn(Opcodes.GOTO, labelForPos(badPc))
                                vis.visitLabel(afterLabel)
                            }
                        case _ =>
                    }

                    val possiblePaths = returnType match {
                        case t if t == classOf[Unit] =>
                            goodPaths
                        case t if t == classOf[Any] =>
                            if (goodPaths.sizeIs <= 1) {
                                goodPaths
                            } else {
                                val Successor(fallThroughPc, fallThroughActions) = instrInfo.fallThroughPath.get

                                if (fallThroughActions.isEmpty) {
                                    vis.visitInsn(Opcodes.DUP)
                                    vis.loadObject(FallthroughMarker)
                                    vis.visitJumpInsn(Opcodes.IF_ACMPEQ, labelForPos(fallThroughPc))
                                } else {
                                    val afterLabel = Label()
                                    vis.visitInsn(Opcodes.DUP)
                                    vis.loadObject(FallthroughMarker)
                                    vis.visitJumpInsn(Opcodes.IF_ACMPNE, afterLabel)
                                    
                                    performActions(fallThroughActions)
                                    
                                    vis.visitJumpInsn(Opcodes.GOTO, labelForPos(fallThroughPc))
                                    vis.visitLabel(afterLabel)
                                }

                                instrInfo.jumpPaths
                            }
                    }

                    require(possiblePaths.sizeIs <= 1)
                    jumpToSuccessors(pos, possiblePaths.map(it => it -> labelForPos(it.pc)))
                }
                
                def performCustomActions(actions: Array[JitImpl.Action]): Unit = {
                    for (action <- actions) {
                        action match {
                            case JitImpl.Action.PushTrue =>
                                vis.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/Boolean", "TRUE", "Ljava/lang/Boolean;")
                            case JitImpl.Action.Swap =>
                                vis.visitInsn(Opcodes.SWAP)
                            case JitImpl.Action.DupX1 =>
                                vis.visitInsn(Opcodes.DUP_X1)
                            case JitImpl.Action.UpdateCheckOffset =>
                                handlerLocal(instrInfo.stackInfo.handlers.head.pc).foreach { local =>
                                    loadContext()
                                    vis.callMethod(Methods.Context.GET_OFFSET)
                                    vis.visitVarInsn(Opcodes.ISTORE, local)
                                }
                        }
                    }
                }

                def applySpecialized(instr: Instr & SpecializedInstr): Unit = {
                    val (method, info) = InstructionImpls.getImpl(instr)
                    val trailingParams = method.getParameterTypes.view.zip(method.getParameterAnnotations).drop(info.consumeOperands).toArray

                    performCustomActions(info.beforeActions)

                    if (info.noop) {
                        performActions(Seq(AfterAction.PopOperands(info.consumeOperands)))
                    } else {
                        info.consumeOperands match {
                            case 0 =>
                                vis.loadObject(instr)
                            case 1 =>
                                vis.loadObject(instr)
                                vis.visitInsn(Opcodes.SWAP)
                            case 2 =>
                                vis.loadObject(instr)
                                vis.visitInsn(Opcodes.DUP_X2)
                                vis.visitInsn(Opcodes.POP)
                            case 3 =>
                                val currentHandlers = instrInfo.stackInfo.handlers.view.map(_.pc)
                                val localOffset = currentHandlers.map(handlerLocal).collectFirst { case Some(local) => local }.getOrElse(0)

                                val toStore = info.consumeOperands - 2
                                for (local <- toStore to 1 by -1) {
                                    vis.visitVarInsn(Opcodes.ASTORE, local + localOffset)
                                }
                                vis.loadObject(instr)
                                vis.visitInsn(Opcodes.DUP_X2)
                                vis.visitInsn(Opcodes.POP)
                                for (local <- 1 to toStore) {
                                    vis.visitVarInsn(Opcodes.ALOAD, local + localOffset)
                                }
                        }

                        for ((ty, annotations) <- trailingParams) {
                            if (ty eq classOf[Context]) {
                                loadContext()
                            } else {
                                require(annotations.view.collect {
                                    case _: Pc => vis.loadInt(pos)
                                    case _: HandlerCheck =>
                                        vis.visitVarInsn(Opcodes.ILOAD, handlerLocal(instrInfo.stackInfo.handlers.head.pc).get)
                                }.sizeIs == 1)
                            }
                        }

                        vis.callMethod(method)
                    }

                    performCustomActions(info.afterActions)

                    performActions(instrInfo.afterActions)

                    jumpUsingReturnValue(method.getReturnType)
                }

                instr match {
                    case Call(id, producesResults) =>
                        loadContext()
                        vis.visitMethodInsn(Opcodes.INVOKESTATIC, className(id), IMPL_NAME, implDesc(producesResults), false)
                        performActions(instrInfo.afterActions)
                        jumpUsingReturnValue(if (producesResults) classOf[AnyRef] else classOf[Boolean])
                    case Case(label) =>
                        val rightLabel = Label()

                        vis.visitInsn(Opcodes.DUP)
                        vis.visitTypeInsn(Opcodes.INSTANCEOF, Type.getInternalName(classOf[Left[?, ?]]))
                        vis.visitJumpInsn(Opcodes.IFEQ, rightLabel)

                        vis.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(classOf[Left[?, ?]]))
                        vis.callMethod(Methods.Either.LEFT_VALUE)
                        vis.visitJumpInsn(Opcodes.GOTO, labelForPos(label))

                        vis.visitLabel(rightLabel)
                        vis.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(classOf[Right[?, ?]]))
                        vis.callMethod(Methods.Either.RIGHT_VALUE)
                    case ManyUntil(label) =>
                        val stopLabel = Label()

                        vis.visitInsn(Opcodes.SWAP)
                        vis.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(classOf[mutable.Builder[?, ?]]))
                        vis.visitInsn(Opcodes.SWAP)

                        vis.visitInsn(Opcodes.DUP)
                        vis.visitTypeInsn(Opcodes.INSTANCEOF, Type.getInternalName(classOf[ManyUntil.Stop.type]))
                        vis.visitJumpInsn(Opcodes.IFNE, stopLabel)

                        vis.callMethod(Methods.Builder.ADD_ONE)
                        vis.visitJumpInsn(Opcodes.GOTO, labelForPos(label))

                        vis.visitLabel(stopLabel)
                        vis.visitInsn(Opcodes.POP)
                        vis.callMethod(Methods.Builder.RESULT)
                    case specialized: SpecializedInstr =>
                        applySpecialized(specialized)
                    case _ =>
                        vis.loadObject(instr)
                        loadContext()
                        vis.loadInt(pos)
                        vis.callMethod(Methods.Instr.APPLY)
                        performActions(instrInfo.afterActions)
                        jumpToAllSuccessors()
                }
            }

            if (function.producesResults) {
                vis.visitLabel(successLabel)
                vis.visitInsn(Opcodes.ARETURN)
                vis.visitLabel(failureLabel)
                vis.loadObject(FailMarker)
                vis.visitInsn(Opcodes.ARETURN)
            } else {
                vis.visitLabel(successLabel)
                vis.visitInsn(Opcodes.ICONST_1)
                vis.visitInsn(Opcodes.IRETURN)
                vis.visitLabel(failureLabel)
                vis.visitInsn(Opcodes.ICONST_0)
                vis.visitInsn(Opcodes.IRETURN)
            }
            vis.visitEnd()
        }

    private def implDesc(producesResults: Boolean) = if (producesResults) IMPL_DESC else VOID_IMPL_DESC

    private def className(id: Int) = s"parsley/internal/machine/jit/gen/parsers/Parser$id"
}