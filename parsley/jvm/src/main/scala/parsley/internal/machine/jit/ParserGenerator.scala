package parsley.internal.machine.jit

import java.lang.invoke.{MethodHandles, MethodType}

import scala.collection.mutable

import parsley.internal.machine.{Context, ParseRunner}
import parsley.internal.machine.instructions.{Call, DynCall, Instr}
import parsley.internal.machine.stacks.Stack.StackExt

import org.objectweb.asm.{Label, Opcodes, Type}

private val IMPL_NAME = "parse"
private val IMPL_DESC = Type.getMethodDescriptor(Type.VOID_TYPE, Type.getType(classOf[Context]))
private val CONTEXT = Type.getType(classOf[Context])

private[jit] class ParserGenerator(private val functions: Array[ParserFunction]) {
    private val ctx = ClassGenContext()
    private val functionsById = functions.view.map(it => it.id -> it).toMap
    private val canFailCache = mutable.Map.empty[Int, Boolean]

    def generate(): ParseRunner = {
        val classes = functions.map(generate)
        val startMethod = MethodHandles.lookup().findStatic(classes.head, IMPL_NAME, MethodType.methodType(Void.TYPE, classOf[Context]))
        new ParseRunner {
            override def run(ctx: Context): Unit = {
                startMethod.invokeExact(ctx)
            }

            override def dynCall(ctx: Context): Unit = {
                ctx.call(0)
                //noinspection ScalaUnusedExpression
                startMethod.invokeExact(ctx): Unit
            }

            override def fail(ctx: Context): Unit = {
                if (ctx.handlers.isEmpty) {
                    ctx.running = false
                    ctx.pc = -1
                } else {
                    val handler = ctx.handlers
                    ctx.instrs = handler.instrs
                    if (ctx.calls eq handler.calls) {
                        // Local handler
                        ctx.pc = handler.pc
                    } else {
                        // Handler in parent method
                        ctx.pc = -1
                    }
                    ctx.calls = handler.calls
                    val diffstack = ctx.stack.usize - handler.stacksz
                    if (diffstack > 0) ctx.stack.drop(diffstack)
                }
            }
        }
    }

    private def generate(function: ParserFunction): Class[?] =
        ctx.newClass(Opcodes.ACC_PUBLIC, className(function.id)) { classVisitor =>
            val vis = classVisitor.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, IMPL_NAME, IMPL_DESC, null, null)
            val instrLabels = function.instrs.map(_ => Label())
            val endLabel = Label()

            def labelForPos(pos: Int) = if (pos == -1) endLabel else instrLabels(pos)

            def jumpToSuccessors(pos: Int, successors: Set[Int]): Unit = {
                successors.size match {
                    case 0 =>
                        vis.visitJumpInsn(Opcodes.GOTO, endLabel)
                    case 1 if successors.contains(pos + 1) =>
                    case 1 =>
                        vis.visitJumpInsn(Opcodes.GOTO, labelForPos(successors.head))
                    case _ =>
                        vis.visitVarInsn(Opcodes.ALOAD, 0)
                        vis.visitMethodInsn(Opcodes.INVOKEVIRTUAL, CONTEXT.getInternalName, "pc", "()I", false)

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

            vis.visitVarInsn(Opcodes.ALOAD, 0)
            vis.loadInt(0)
            vis.visitMethodInsn(Opcodes.INVOKEVIRTUAL, CONTEXT.getInternalName, "pc_$eq", "(I)V", false)

            for ((instr, pos) <- function.instrs.view.zipWithIndex) {
                val successors = function.successorInfos(pos)

                vis.visitLabel(instrLabels(pos))

                vis.loadObject(instr)

//                vis.visitInsn(Opcodes.DUP)
//                vis.loadInt(pos)
//                vis.visitVarInsn(Opcodes.ALOAD, 0)
//                vis.visitMethodInsn(Opcodes.INVOKESTATIC, JIT_RUNTIME, "beforeInstruction", "(Ljava/lang/Object;ILjava/lang/Object;)V", false)

                vis.visitVarInsn(Opcodes.ALOAD, 0)
                vis.visitMethodInsn(
                    Opcodes.INVOKEVIRTUAL,
                    Type.getType(classOf[Instr]).getInternalName,
                    "apply",
                    Type.getMethodDescriptor(Type.VOID_TYPE, CONTEXT),
                    false
                )

                instr match {
                    case Call(id) =>
                        vis.visitVarInsn(Opcodes.ALOAD, 0)
                        vis.visitMethodInsn(Opcodes.INVOKESTATIC, className(id), IMPL_NAME, IMPL_DESC, false)
                    case _ =>
                }

                instr match {
                    case _: (Call | DynCall) =>
                        assume(successors.goodPaths == Set(pos + 1) && successors.badPaths.sizeIs == 1)

                        val mightFail = instr match {
                            case Call(id) => canFail(id)
                            case _ => true
                        }

                        if (mightFail) {
                            val goodLabel = Label()

                            vis.visitVarInsn(Opcodes.ALOAD, 0)
                            vis.visitMethodInsn(Opcodes.INVOKEVIRTUAL, CONTEXT.getInternalName, "good", "()Z", false)
                            vis.visitJumpInsn(Opcodes.IFNE, goodLabel)

                            // Bad case
                            vis.visitVarInsn(Opcodes.ALOAD, 0)
                            vis.loadInt(successors.badPaths.head)
                            vis.visitMethodInsn(Opcodes.INVOKEVIRTUAL, CONTEXT.getInternalName, "pc_$eq", "(I)V", false)
                            vis.visitJumpInsn(Opcodes.GOTO, labelForPos(successors.badPaths.head))

                            vis.visitLabel(goodLabel)
                        }

                        vis.visitVarInsn(Opcodes.ALOAD, 0)
                        vis.loadInt(pos + 1)
                        vis.visitMethodInsn(Opcodes.INVOKEVIRTUAL, CONTEXT.getInternalName, "pc_$eq", "(I)V", false)
                    case _ =>
                        jumpToSuccessors(pos, successors.combined)
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
                case _ => successors.badPaths.nonEmpty
            }
        }
    }

    private def className(id: Int) = s"parsley/internal/machine/jit/gen/parsers/Parser$id"
}