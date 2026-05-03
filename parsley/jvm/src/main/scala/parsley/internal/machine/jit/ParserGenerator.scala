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
private val IMPL_DESC = Type.getMethodDescriptor(Type.BOOLEAN_TYPE, JIT_CONTEXT)

private object Methods {
    object Context {
        val IS_GOOD: Method = classOf[JitContext].getMethod("good")
    }

    object Instr {
        val APPLY: Method = classOf[Instr].getMethod("apply", classOf[Context], classOf[Int])
    }
}

private[jit] class ParserGenerator(private val functions: Array[ParserFunction]) {
    private val ctx = ClassGenContext()
    private val functionsById = functions.view.map(it => it.id -> it).toMap
    private val canFailCache = mutable.Map.empty[Int, Boolean]

    def generate(): MethodHandle = {
        val classes = functions.map(generate)
        MethodHandles.lookup().findStatic(classes.head, IMPL_NAME, MethodType.methodType(classOf[Boolean], classOf[JitContext]))
    }

    private def generate(function: ParserFunction): Class[?] =
        ctx.newClass(Opcodes.ACC_PUBLIC, className(function.id)) { classVisitor =>
            val vis = classVisitor.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, IMPL_NAME, IMPL_DESC, null, null)
            val instrLabels = function.instrs.map(_ => Label())
            val successLabel = Label()
            val failureLabel = Label()

            def loadContext(): Unit = vis.visitVarInsn(Opcodes.ALOAD, 0)

            def labelForPos(pos: Int) = if (pos == -1) failureLabel else instrLabels(pos)

            def jumpToSuccessors(pos: Int, successors: Map[Int, Label]): Unit = {
                successors.size match {
                    case 0 =>
                        vis.visitInsn(Opcodes.POP)
                        if (pos != function.instrs.indices.last) {
                            vis.visitJumpInsn(Opcodes.GOTO, successLabel)
                        }
                    case 1 if successors.contains(pos + 1) =>
                        vis.visitInsn(Opcodes.POP)
                    case 1 =>
                        vis.visitInsn(Opcodes.POP)
                        vis.visitJumpInsn(Opcodes.GOTO, successors.head._2)
                    case _ =>
                        successors.size match {
                            case 2 if successors.contains(pos + 1) =>
                                vis.loadInt(pos + 1)
                                vis.visitJumpInsn(Opcodes.IF_ICMPNE, successors.find(_._1 != pos + 1).get._2)
                            case 2 =>
                                val Seq(a, b) = successors.toSeq
                                vis.loadInt(a._1)
                                vis.visitJumpInsn(Opcodes.IF_ICMPEQ, a._2)
                                vis.visitJumpInsn(Opcodes.GOTO, b._2)
                            case _ =>
                                val keys = successors.toArray
                                keys.sortInPlaceBy(_._1)
                                val (first, rest) = (keys.head, keys.tail)
                                vis.visitLookupSwitchInsn(first._2, rest.map(_._1), rest.map(_._2))
                        }
                }
            }

            for ((instr, pos) <- function.instrs.view.zipWithIndex if function.successorInfos(pos).isReachable) {
                val successors = function.successorInfos(pos)

                vis.visitLabel(instrLabels(pos))

//                vis.loadObject(instr)
//                vis.loadInt(pos)
//                vis.visitVarInsn(Opcodes.ALOAD, 0)
//                vis.visitMethodInsn(Opcodes.INVOKESTATIC, JIT_RUNTIME, "beforeInstruction", "(Ljava/lang/Object;ILjava/lang/Object;)V", false)

                instr match {
                    case _: Call =>
                    case Return | Halt =>
                        // No-ops
                        vis.loadInt(-1)
                    case _ =>
                        vis.loadObject(instr)
                        loadContext()
                        vis.loadInt(pos)
                        vis.callMethod(Methods.Instr.APPLY)
                }

                instr match {
                    case Call(id) =>
                        loadContext()
                        vis.visitMethodInsn(Opcodes.INVOKESTATIC, className(id), IMPL_NAME, IMPL_DESC, false)
                        if (canFail(id)) {
                            vis.visitJumpInsn(Opcodes.IFEQ, labelForPos(successors.badPath.get))
                        } else {
                            vis.visitInsn(Opcodes.POP)
                        }
                    case _ =>
                        val successorLabels = Map.newBuilder[Int, Label]

                        successorLabels ++= successors.goodPaths.map(it => it -> labelForPos(it))
                        successorLabels ++= successors.badPath.map(-1 -> labelForPos(_))

                        jumpToSuccessors(pos, successorLabels.result())
                }
            }

            vis.visitLabel(successLabel)
            vis.visitInsn(Opcodes.ICONST_1)
            vis.visitInsn(Opcodes.IRETURN)
            vis.visitLabel(failureLabel)
            vis.visitInsn(Opcodes.ICONST_0)
            vis.visitInsn(Opcodes.IRETURN)
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

    private def className(id: Int) = s"parsley/internal/machine/jit/gen/parsers/Parser$id"
}