package parsley.internal.machine.jit.codegen

import scala.collection.mutable

import parsley.internal.machine.jit.ClassGenContext

import org.objectweb.asm.{Label, Opcodes}

private[codegen] case class JumpPath(indicator: Int, label: Label, afterAction: Option[() => Unit])

private[codegen] object CodeGenUtils {
    def jumpDispatch(successors: Seq[JumpPath], fallThroughLabel: Label)(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        if (successors.size <= 1) {
            vis.visitInsn(Opcodes.POP)
            jumpDispatch(successors.headOption, fallThroughLabel)
            return
        }
        successors match {
            case Seq(path1, path2) if (path1.label eq fallThroughLabel) || (path2.label eq fallThroughLabel) =>
                val (JumpPath(fallThroughPc, _, fallThroughActions), JumpPath(_, jumpLabel, jumpActions)) =
                    if (path1.label eq fallThroughLabel) (path1, path2) else (path2, path1)

                vis.loadInt(fallThroughPc)
                if (jumpActions.isEmpty) {
                    vis.visitJumpInsn(Opcodes.IF_ICMPNE, jumpLabel)
                } else {
                    val fallThroughLabel = new Label()
                    vis.visitJumpInsn(Opcodes.IF_ICMPEQ, fallThroughLabel)
                    performActions(jumpActions)
                    vis.visitJumpInsn(Opcodes.GOTO, jumpLabel)
                    vis.visitLabel(fallThroughLabel)
                }
                performActions(fallThroughActions)
            case Seq(successor1@JumpPath(_, label1, _), successor2@JumpPath(_, label2, _)) =>
                vis.loadInt(successor1.indicator)

                (successor1.afterAction, successor2.afterAction) match {
                    case (None, None) =>
                        vis.visitJumpInsn(Opcodes.IF_ICMPEQ, label1)
                        vis.visitJumpInsn(Opcodes.GOTO, label2)
                    case (afterActions, None) =>
                        vis.visitJumpInsn(Opcodes.IF_ICMPNE, label2)
                        performActions(afterActions)
                        vis.visitJumpInsn(Opcodes.GOTO, label1)
                    case (None, afterActions) =>
                        vis.visitJumpInsn(Opcodes.IF_ICMPEQ, label1)
                        performActions(afterActions)
                        vis.visitJumpInsn(Opcodes.GOTO, label2)
                    case (afterActions1, afterActions2) =>
                        val successor1Label = new Label()
                        vis.visitJumpInsn(Opcodes.IF_ICMPEQ, successor1Label)
                        performActions(afterActions2)
                        vis.visitJumpInsn(Opcodes.GOTO, label2)
                        vis.visitLabel(successor1Label)
                        performActions(afterActions1)
                        vis.visitJumpInsn(Opcodes.GOTO, label1)
                }
            case _ =>
                val keys = successors.sortBy(_.indicator).toArray
                val afterSwitchTasks = mutable.Buffer.empty[() => Unit]

                val switchLabels = keys.map {
                    case JumpPath(_, label, None) => label
                    case JumpPath(_, label, afterActions) =>
                        val newLabel = new Label()
                        afterSwitchTasks += (() => {
                            vis.visitLabel(newLabel)
                            performActions(afterActions)
                            vis.visitJumpInsn(Opcodes.GOTO, label)
                        })
                        newLabel
                }

                vis.visitLookupSwitchInsn(switchLabels.last, keys.view.init.map(_.indicator).toArray, switchLabels.init)

                for (task <- afterSwitchTasks) {
                    task()
                }
        }
    }

    def jumpDispatch(successor: Option[JumpPath], fallThroughLabel: Label)(implicit vis: ClassGenContext#MethodGenVisitor): Unit =
        successor match {
            case None =>
                vis.visitInsn(Opcodes.ACONST_NULL)
                vis.visitInsn(Opcodes.ATHROW)
            case Some(JumpPath(_, nextLabel, afterActions)) =>
                afterActions.foreach(_())
                if (nextLabel ne fallThroughLabel) {
                    vis.visitJumpInsn(Opcodes.GOTO, nextLabel)
                }
        }

    def goToLabel(label: Label, fallThroughLabel: Label)(implicit vis: ClassGenContext#MethodGenVisitor): Unit = {
        if (label ne fallThroughLabel) {
            vis.visitJumpInsn(Opcodes.GOTO, label)
        }
    }

    private def performActions(actions: Iterable[() => Unit]): Unit = actions.foreach(_())
}
