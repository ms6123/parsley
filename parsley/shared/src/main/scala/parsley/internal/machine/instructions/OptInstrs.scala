/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import scala.annotation.tailrec
import scala.collection.mutable

import parsley.XCompat.*
import parsley.token.errors.LabelConfig

import parsley.internal.errors.ExpectItem
import parsley.internal.machine.Context
import parsley.internal.machine.XAssert.*
import parsley.internal.machine.errors.{EmptyHints, ExpectedError}
import parsley.internal.machine.stacks.ErrorStack

private [internal] final class Lift1(f: Any => Any) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.exchangeAndContinue(f(ctx.stack.upeek))
    }
    // $COVERAGE-OFF$
    override def toString: String = "Perform(?)"
    // $COVERAGE-ON$

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}
private [internal] object Lift1 {
    def apply[A, B](f: A => B): Lift1 = new Lift1(f.asInstanceOf[Any => Any])
}

private [internal] final class Exchange[A](private [Exchange] val x: A) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.exchangeAndContinue(x)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Ex($x)"
    // $COVERAGE-ON$

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [internal] final class SatisfyExchange[A](f: Char => Boolean, x: A, _expected: LabelConfig) extends Instr {
    private [this] final val expected = _expected.asExpectDescs
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        if (ctx.moreInput && f(ctx.peekChar)) {
            ctx.consumeChar()
            ctx.pushAndContinue(x)
        }
        else ctx.expectedFail(expected, unexpectedWidth = 1)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"SatEx(?, $x)"
    // $COVERAGE-ON$
}

private [internal] final class RecoverWith[A](x: A) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.restoreHints() // This must be before adding the error to hints
        ctx.catchNoConsumed(ctx.handlers.check) {
            ctx.popHandler()
            ctx.addErrorToHintsAndPop()
            ctx.pushAndContinue(x)
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"RecoverWith($x)"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = Some(handlers.tail)

    override def failPath(handlers: List[Int]): Option[List[Int]] = Some(handlers.tail)
}

private [internal] final class AlwaysRecoverWith[A](x: A) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.restoreState()
        ctx.restoreHints() // This must be before adding the error to hints
        ctx.popHandler()
        ctx.addErrorToHintsAndPop()
        ctx.good = true
        ctx.pushAndContinue(x)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"AlwaysRecoverWith($x)"
    // $COVERAGE-ON$

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = Some(handlers.tail)

    override def failPath(handlers: List[Int]): Option[List[Int]] = None
}

private [internal] sealed abstract class JumpTablePreds {
    val next: JumpTablePreds
    def relabelThis(labels: Int => Int): Unit
    protected def ownLabels: Seq[Int]
    def toPartialFunction: PartialFunction[Char, (Int, Iterable[ExpectItem])]

    @tailrec
    final def relabel(labels: Int => Int): Unit = {
        this.relabelThis(labels)
        if (next ne null) next.relabel(labels)
    }

    final def labels: Seq[Int] = ownLabels ++ (if (next ne null) next.labels else Nil)

    final def toPartialFunctions: List[PartialFunction[Char, (Int, Iterable[ExpectItem])]] = toPartialFunctions(mutable.ListBuffer.empty)
    @tailrec
    private def toPartialFunctions(fns: mutable.ListBuffer[PartialFunction[Char, (Int, Iterable[ExpectItem])]]): List[PartialFunction[Char, (Int, Iterable[ExpectItem])]] = {
        fns += this.toPartialFunction
        if (next ne null) next.toPartialFunctions(fns)
        else fns.toList
    }
}
private [internal] object JumpTablePreds {
    def fromList(preds: List[Either[mutable.Map[Char, (Int, Iterable[ExpectItem])], (Char => Boolean, Int, Iterable[ExpectItem])]]): JumpTablePreds =
        preds.foldRight[JumpTablePreds](null) {
            case (Left(map), preds) => new JumpTableCharMapPred(map, preds)
            case (Right((pred, label, errs)), preds) => new JumpTableCharFunPred(pred, label, errs, preds)
        }
}
private [internal] final class JumpTableCharMapPred(val map: mutable.Map[Char, (Int, Iterable[ExpectItem])], val next: JumpTablePreds) extends JumpTablePreds {
    def relabelThis(labels: Int => Int): Unit = {
        val _ = map.mapValuesInPlaceCompat { case (_, (i, errs)) => (labels(i), errs) }
    }
    override protected def ownLabels: Seq[Int] = map.values.map(_._1).toSeq
    def toPartialFunction: PartialFunction[Char, (Int, Iterable[ExpectItem])] = map.toMap

    // $COVERAGE-OFF$
    override def toString: String = s"${map.toList.sortBy{case (_, (l, _)) => l}.map{case (k, v) => s"${k.toChar} -> ${v._1}"}.mkString(", ")}${if (next ne null) s", $next" else ""}"
    // $COVERAGE-ON$
}
private [internal] final class JumpTableCharFunPred(val pred: Char => Boolean, var label: Int, val errors: Iterable[ExpectItem], val next: JumpTablePreds) extends JumpTablePreds{
    def relabelThis(labels: Int => Int): Unit = this.label = labels(this.label)
    override protected def ownLabels: Seq[Int] = Seq(label)
    def toPartialFunction: PartialFunction[Char, (Int, Iterable[ExpectItem])] = {
        val labelErrs = (label, errors)

        { case (c: Char) if pred(c) => labelErrs }
    }

    // $COVERAGE-OFF$
    override def toString: String =  s"?(_) -> $label${if (next ne null) s", $next" else ""}"
    // $COVERAGE-ON$
}

private [internal] final class JumpTable
    (jumpTable: JumpTablePreds, private [this] var default: Int, private [this] var defaultMergeHandler: Int, private [this] var individualMergeHandler: Int, size: Int, allErrorItems: Iterable[ExpectItem]) extends Instr {
    private [this] var defaultPreamble: Int = _
    private [this] var jumpTableFuncs: List[PartialFunction[Char, (Int, Iterable[ExpectItem])]] = _

    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        if (ctx.moreInput) {
            val (dest, errorItems) = getRoot(ctx.peekChar, jumpTableFuncs)
            ctx.pc = dest
            if (dest != default) {
                ctx.pushHandler(defaultPreamble)
                ctx.pushHandler(individualMergeHandler)
                ctx.hints = EmptyHints
            } else {
                ctx.pushHandler(defaultMergeHandler)
            }
            addErrors(ctx, errorItems)
        }
        else {
            addErrors(ctx, allErrorItems)
            ctx.pushHandler(defaultMergeHandler)
            ctx.pc = default
        }
    }

    // This is using the same trick that the standard library uses, under the assumption that `applyOrElse` has been optimised to
    // prevent double evaluation (which should be the case).
    // While we could use the same kind of dummy value, we do have the luxury of knowing that the result is non-null, so null
    // can serve as an appropriate marker. After we've confirmed non-null, then we can do the recursion
    @tailrec
    private def getRoot(char: Char, fss: List[PartialFunction[Char, (Int, Iterable[ExpectItem])]]): (Int, Iterable[ExpectItem]) = fss match {
        case Nil => (default, allErrorItems)
        case f :: fs =>
            val res = f.applyOrElse(char, JumpTable.checkDefined)
            if (JumpTable.wasUndefined(res)) getRoot(char, fs) else res
    }

    private def addErrors(ctx: Context, errorItems: Iterable[ExpectItem]): Unit = {
        // FIXME: the more appropriate way of demanding input may be to pick 1 character, for same rationale with StringTok
        ctx.errs.push(new ExpectedError(ctx.offset, ctx.line, ctx.col, errorItems, unexpectedWidth = size))
    }

    override def relabel(labels: Int => Int): this.type = {
        jumpTable.relabel(labels)
        default = labels(default)
        defaultMergeHandler = labels(defaultMergeHandler)
        individualMergeHandler = labels(individualMergeHandler)
        defaultPreamble = default - 1
        jumpTableFuncs = jumpTable.toPartialFunctions
        this
    }

    // $COVERAGE-OFF$
    override def toString: String = s"JumpTable($jumpTable, _ -> $default, $defaultMergeHandler, $individualMergeHandler)"
    // $COVERAGE-ON$

    override def labels: Seq[Int] = defaultPreamble +: defaultMergeHandler +: individualMergeHandler +: default +: jumpTable.labels

    override def fallThroughPath(handlers: List[Int]): Option[List[Int]] = None

    override def failPath(handlers: List[Int]): Option[List[Int]] = None

    override def jumpPaths(handlers: List[Int]): Seq[(List[Int], Int)] =
        ((defaultMergeHandler :: handlers) -> default) +:
            jumpTable.labels.map((individualMergeHandler :: defaultPreamble :: handlers) -> _)
}
private [instructions] object JumpTable {
    private val checkDefined = (_: Any) => null
    private def wasUndefined[B <: AnyRef](x: B) = null eq x
}
