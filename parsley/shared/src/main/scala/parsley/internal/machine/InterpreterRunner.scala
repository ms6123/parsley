package parsley.internal.machine

import parsley.errors.ErrorBuilder

import parsley.internal.machine.instructions.Instr

import parsley.Result

private [parsley] class InterpreterRunner(val instrs: Array[Instr], numRegs: Int) extends ParseRunner {
    override def run[Err: ErrorBuilder, A](input: String, sourceFile: Option[String]): Result[Err, A] =
        new InterpreterContext(instrs, input, numRegs, sourceFile).run()

    override def dynCall(ctx: Context, pc: Int, continuation: AnyRef): Null =
        ctx match {
            case ctx: InterpreterContext =>
                ctx.call(0)
                ctx.instrs = instrs
                null
        }
}
