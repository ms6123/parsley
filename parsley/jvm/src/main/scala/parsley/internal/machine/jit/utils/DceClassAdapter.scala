/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.jit.utils

import org.objectweb.asm.{ClassVisitor, MethodVisitor, Opcodes}

private[jit] class DceClassAdapter(next: ClassVisitor) extends ClassVisitor(Opcodes.ASM9, next) {
    override def visitMethod(access: Int,
                             name: String,
                             desc: String,
                             signature: String,
                             exceptions: Array[String]): MethodVisitor =
        new DceMethodAdapter(access, name, desc, signature, exceptions, super.visitMethod(access, name, desc, signature, exceptions))
}
