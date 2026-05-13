/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions.token

import parsley.internal.machine.Context

@FunctionalInterface
private [parsley] trait CharPredicate {
    def peek(ctx: Context): Boolean
}

private [parsley] object CharPredicate {
    def basic(f: Char => Boolean): CharPredicate = ctx => ctx.moreInput && f(ctx.peekChar)

    def unicode(f: Int => Boolean): CharPredicate = ctx => {
        lazy val hc = ctx.peekChar(0)
        lazy val l = ctx.peekChar(1)
        ctx.moreInput(2) && hc.isHighSurrogate && Character.isSurrogatePair(hc, l) && f(Character.toCodePoint(hc, l)) || ctx.moreInput && f(hc.toInt)
    }

    val notRequired: CharPredicate = _ => false
}
