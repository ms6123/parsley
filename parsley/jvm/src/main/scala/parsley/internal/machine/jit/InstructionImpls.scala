/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.jit

import java.lang.reflect.Method

import scala.collection.mutable

import parsley.internal.machine.instructions.{Instr, JitImpl, SpecializedInstr}

object InstructionImpls {
    private val cache = mutable.Map.empty[Class[?], (Method, JitImpl)]

    def getImpl(instr: SpecializedInstr): (Method, JitImpl) =
        cache.getOrElseUpdate(instr.getClass, {
            instr.getClass.getMethods.view.map(it => it -> it.getAnnotation(classOf[JitImpl])).find(_._2 ne null).getOrElse {
                throw new UnsupportedOperationException(s"No JIT implementation found for ${instr.getClass.getName}")
            }
        })
}
