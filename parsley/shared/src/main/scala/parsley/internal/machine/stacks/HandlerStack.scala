/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.stacks

import parsley.internal.machine.errors.DefuncHints

private [machine] abstract class HandlerStack {
    def stacksz: Int
    def check: Int
    def check_=(v: Int): Unit
    def hints: DefuncHints
    def hintOffset: Int
}