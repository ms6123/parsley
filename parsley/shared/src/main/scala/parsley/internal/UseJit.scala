/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal

import parsley.internal.machine.ParseRunner

sealed trait UseJit

object UseJit {
    case class Yes(fallback: () => ParseRunner) extends UseJit
    case object No extends UseJit
}
