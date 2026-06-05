/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import parsley.internal.machine.{Context, InterpreterContext}

import org.typelevel.scalaccompat.annotation.unused

private [internal] abstract class Instr {
    def apply(ctx: Context, pc: Int): Int
    def relabel(@unused labels: Int => Int): this.type = this
    // Instructions should override this if they have mutable state inside!
    def copy: Instr = this

    def labels: Seq[Int] = Seq.empty

    def fallThroughPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers))
    def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(stacksz, handlers))
    def jumpPaths(@unused stacksz: Int, @unused handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] = Seq.empty

    final def allPaths(stacksz: Int, handlers: List[HandlerInfo], pos: Int): Seq[(Int, StackInfo)] =
        goodPaths(stacksz, handlers, pos) ++ badPaths(stacksz, handlers)
        
    final def goodPaths(stacksz: Int, handlers: List[HandlerInfo], pos: Int): Seq[(Int, StackInfo)] =
        fallThroughPath(stacksz, handlers).map((pos + 1) -> _).toSeq ++
            jumpPaths(stacksz, handlers)
        
    final def badPaths(stacksz: Int, handlers: List[HandlerInfo]): Seq[(Int, StackInfo)] =
        failPath(stacksz, handlers).map { newStack =>
            val activeHandler = newStack.handlers.head
            activeHandler.pc -> StackInfo(math.min(activeHandler.stacksz, newStack.stacksz), newStack.handlers)
        }.toSeq
}

private [internal] case class StackInfo(stacksz: Int, handlers: List[HandlerInfo])

private [internal] case class HandlerInfo(pc: Int, stacksz: Int)

private [internal] trait SpecializedInstr {
    this: Instr =>
    
    def apply(ctx: InterpreterContext, pc: Int): Int

    final override def apply(ctx: Context, pc: Int): Int = {
        ctx match {
            case ctx: InterpreterContext => 
                apply(ctx, pc)
            case _ => throw new IllegalArgumentException(s"$this needs to be specialized for context type ${ctx.getClass.getName}")
        }
    }
}

private [internal] trait IntrinsicInstr extends SpecializedInstr {
    this: Instr =>

    @JitImpl
    def apply(): Unit = throw new UnsupportedOperationException("Intrinsic")
}

private [internal] abstract class InstrWithLabel extends Instr {
    var label: Int
    override def relabel(labels: Int => Int): this.type = {
        label = labels(label)
        this
    }

    override def copy: Instr

    override def labels: Seq[Int] = Seq(label)
}

// It's 2018 and Labels are making a come-back, along with 2 pass assembly
private [internal] final class Label(val i: Int) extends Instr {
    // $COVERAGE-OFF$
    def apply(ctx: Context, pc: Int): Int = throw new Exception("Cannot execute label") // scalastyle:ignore throw
    // $COVERAGE-ON$
}

private [internal] trait RefailInstr extends SpecializedInstr {
    this: Instr =>

    def failStacksz(stacksz: Int): Int = stacksz

    final override def fallThroughPath(@unused stacksz: Int, @unused handlers: List[HandlerInfo]): Option[StackInfo] = None

    final override def failPath(stacksz: Int, handlers: List[HandlerInfo]): Option[StackInfo] = Some(StackInfo(failStacksz(stacksz), handlers.tail))
}