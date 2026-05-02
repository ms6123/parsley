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
private val IMPL_DESC = Type.getMethodDescriptor(Type.VOID_TYPE, JIT_CONTEXT)

private object Methods {
    object Context {
        val GET_PC: Method = classOf[JitContext].getMethod("pc")
        val SET_PC: Method = classOf[JitContext].getMethod("pc_$eq", classOf[Int])
        val IS_GOOD: Method = classOf[JitContext].getMethod("good")
    }

    object Instr {
        val APPLY: Method = classOf[Instr].getMethod("apply", classOf[Context])
    }
}

private[jit] class ParserGenerator(private val functions: Array[ParserFunction]) {
    private val ctx = ClassGenContext()
    private val functionsById = functions.view.map(it => it.id -> it).toMap
    private val canFailCache = mutable.Map.empty[Int, Boolean]

    def generate(): MethodHandle = {
        val classes = functions.map(generate)
        MethodHandles.lookup().findStatic(classes.head, IMPL_NAME, MethodType.methodType(Void.TYPE, classOf[JitContext]))
    }

    private def generate(function: ParserFunction): Class[?] =
        ctx.newClass(Opcodes.ACC_PUBLIC, className(function.id)) { classVisitor =>
            val vis = classVisitor.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, IMPL_NAME, IMPL_DESC, null, null)
            val instrLabels = function.instrs.map(_ => Label())
            val endLabel = Label()

            def loadContext(): Unit = vis.visitVarInsn(Opcodes.ALOAD, 0)

            def labelForPos(pos: Int) = if (pos == -1) endLabel else instrLabels(pos)

            def jumpToSuccessors(pos: Int, successors: Set[Int]): Unit = {
                successors.size match {
                    case 0 =>
                        vis.visitJumpInsn(Opcodes.GOTO, endLabel)
                    case 1 if successors.contains(pos + 1) =>
                    case 1 =>
                        vis.visitJumpInsn(Opcodes.GOTO, labelForPos(successors.head))
                    case _ =>
                        loadContext()
                        vis.callMethod(Methods.Context.GET_PC)

                        successors.size match {
                            case 2 if successors.contains(pos + 1) =>
                                vis.loadInt(pos + 1)
                                vis.visitJumpInsn(Opcodes.IF_ICMPNE, labelForPos(successors.find(_ != pos + 1).get))
                            case 2 =>
                                val Seq(a, b) = successors.toSeq
                                vis.loadInt(a)
                                vis.visitJumpInsn(Opcodes.IF_ICMPEQ, labelForPos(a))
                                vis.visitJumpInsn(Opcodes.GOTO, labelForPos(b))
                            case _ =>
                                val keys = successors.toArray
                                keys.sortInPlace()
                                val (first, rest) = (keys.head, keys.tail)
                                vis.visitLookupSwitchInsn(labelForPos(first), rest, rest.map(labelForPos))
                        }
                }
            }

            loadContext()
            vis.loadInt(0)
            vis.callMethod(Methods.Context.SET_PC)

            for ((instr, pos) <- function.instrs.view.zipWithIndex if function.successorInfos(pos).isReachable) {
                val successors = function.successorInfos(pos)

                vis.visitLabel(instrLabels(pos))

                if (successors.isHandler && successors.goodPaths.nonEmpty) {
                    loadContext()
                    vis.loadInt(pos)
                    vis.callMethod(Methods.Context.SET_PC)
                }

//                vis.loadObject(instr)
//                vis.loadInt(pos)
//                vis.visitVarInsn(Opcodes.ALOAD, 0)
//                vis.visitMethodInsn(Opcodes.INVOKESTATIC, JIT_RUNTIME, "beforeInstruction", "(Ljava/lang/Object;ILjava/lang/Object;)V", false)

                instr match {
                    case _: Call | Return | Halt =>
                        // No-ops
                    case _ =>
                        vis.loadObject(instr)
                        loadContext()
                        vis.callMethod(Methods.Instr.APPLY)
                }

                instr match {
                    case Call(id) =>
                        loadContext()
                        vis.visitMethodInsn(Opcodes.INVOKESTATIC, className(id), IMPL_NAME, IMPL_DESC, false)
                    case _ =>
                }

                val mightFail = instr match {
                    case Call(id) => canFail(id)
                    case _ => successors.badPath.isDefined
                }

                var finalJumpNeeded = true

                if (mightFail) {
                    if (successors.goodPaths.nonEmpty && successors.goodPaths != Set(successors.badPath.get)) {
                        loadContext()
                        vis.callMethod(Methods.Context.IS_GOOD)
                        vis.visitJumpInsn(Opcodes.IFEQ, labelForPos(successors.badPath.get))
                    } else {
                        jumpToSuccessors(pos, Set(successors.badPath.get))
                        finalJumpNeeded = false
                    }
                }

                instr match {
                    case _: (Call | DynCall) =>
                        require(successors.goodPaths == Set(pos + 1))

                        if (!willSetPc(function.instrs(pos + 1), function.successorInfos(pos + 1))) {
                            loadContext()
                            vis.loadInt(pos + 1)
                            vis.callMethod(Methods.Context.SET_PC)
                        }
                    case _ =>
                        if (finalJumpNeeded) {
                            jumpToSuccessors(pos, successors.goodPaths)
                        }
                }
            }

            vis.visitLabel(endLabel)
            vis.visitInsn(Opcodes.RETURN)
            vis.visitEnd()
        }

    private def canFail(id: Int, visited: Set[Int] = Set.empty): Boolean = if (canFailCache.contains(id)) canFailCache(id) else {
        val result = canFailImpl(id, visited)
        canFailCache(id) = result
        result
    }

    private def canFailImpl(id: Int, visited: Set[Int] = Set.empty): Boolean = {
        val func = functionsById(id)
        func.instrs.view.zip(func.successorInfos).exists { case (instr, successors) =>
            instr match {
                case Call(id) => !visited.contains(id) && canFail(id, visited.incl(id))
                case _ => successors.badPath.isDefined
            }
        }
    }

    private def willSetPc(instr: Instr, successors: SuccessorInfo): Boolean =
        instr match {
            case _: (Call | DynCall) => true
            case _ => successors.goodPaths.isEmpty
        }

    private def className(id: Int) = s"parsley/internal/machine/jit/gen/parsers/Parser$id"
}