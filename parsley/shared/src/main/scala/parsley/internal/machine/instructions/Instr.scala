/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import parsley.internal.machine.Context

import org.typelevel.scalaccompat.annotation.unused

private [internal] abstract class Instr {
    def apply(ctx: Context): Unit
    def relabel(@unused labels: Int => Int): this.type = this
    // Instructions should override this if they have mutable state inside!
    def copy: Instr = this

    def labels: Seq[Int] = Seq.empty

    def fallThroughPath(handlers: List[Int]): Option[List[Int]] = Some(handlers)
    def failPath(handlers: List[Int]): Option[List[Int]] = Some(handlers)
    def jumpPaths(@unused handlers: List[Int]): Seq[(List[Int], Int)] = Seq.empty

    final def allPaths(handlers: List[Int], pos: Int): Seq[(List[Int], Int)] =
        fallThroughPath(handlers).map(_ -> (pos + 1)).toSeq ++
            failPath(handlers).map(newHandlers => newHandlers -> newHandlers.head).toSeq ++
            jumpPaths(handlers)
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
    def apply(ctx: Context): Unit = throw new Exception("Cannot execute label") // scalastyle:ignore throw
    // $COVERAGE-ON$
}

private [internal] trait RefailInstr {
    this: Instr =>

    final override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = None

    final override def failPath(handlers: List[Int]): Option[List[Int]] = Some(handlers.tail)
}