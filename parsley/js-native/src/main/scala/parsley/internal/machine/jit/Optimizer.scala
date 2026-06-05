/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.jit

import parsley.internal.machine.ParseRunner

object Optimizer {
  val isEnabled = false
  val useTco = true
  val allowInlining = true
  
  def optimize(params: Any*): ParseRunner = ???
}
