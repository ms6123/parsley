/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.jit.codegen

import java.lang.invoke.{MethodHandle, MethodHandles, MethodType}

import parsley.internal.machine.jit.*
import parsley.internal.machine.jit.codegen.FunctionGenerator.Constants.IMPL_NAME
import parsley.internal.machine.jit.codegen.ParserGenerator.{className, Constants}

import org.objectweb.asm.{Opcodes, Type}

private[jit] class ParserGenerator(private val functions: Array[ParserFunction]) {
    private val ctx = new ClassGenContext()
    private val functionsById = functions.map(it => it.id -> it).toMap

    def generate(): Class[?] = {
        val classes = functions.map(generate)
        classes.head
    }

    private [codegen] def resolveCall(id: Int): ParserFunction = functionsById(id)

    private def generate(function: ParserFunction): Class[?] =
        ctx.newClass(
            Opcodes.ACC_PUBLIC, className(function.id),
            superName = if (function.isCyclic) Constants.CONTINUATION.getInternalName else "java/lang/Object"
        ) { classVisitor =>
            if (function.isCyclic) {
                new StateMachineFunctionGenerator(function, this, classVisitor).generate()
            } else {
                new PlainFunctionGenerator(function, this, classVisitor).generate()
            }
        }
}

private [codegen] object ParserGenerator {
    def className(id: Int): String = s"parsley/internal/machine/jit/gen/parsers/Parser$id"

    object Constants {
        val CONTINUATION: Type = Type.getType(classOf[Continuation])
    }
}