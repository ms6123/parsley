package parsley.internal.machine.jit.codegen

import java.lang.invoke.{MethodHandle, MethodHandles, MethodType}

import parsley.internal.machine.jit.*
import parsley.internal.machine.jit.codegen.FunctionGenerator.Constants.IMPL_NAME
import parsley.internal.machine.jit.codegen.ParserGenerator.className

import org.objectweb.asm.Opcodes

private[jit] class ParserGenerator(private val functions: Array[ParserFunction]) {
    private val ctx = new ClassGenContext()

    def generate(): MethodHandle = {
        val classes = functions.map(generate)
        MethodHandles.lookup().findStatic(classes.head, IMPL_NAME, MethodType.methodType(classOf[AnyRef], classOf[JitContext]))
    }

    private def generate(function: ParserFunction): Class[?] =
        ctx.newClass(Opcodes.ACC_PUBLIC, className(function.id)) { classVisitor =>
            new PlainFunctionGenerator(function, this, classVisitor).generate()
        }
}

private [codegen] object ParserGenerator {
    def className(id: Int): String = s"parsley/internal/machine/jit/gen/parsers/Parser$id"
}