package parsley.internal.machine.jit.codegen

import java.lang.invoke.{MethodHandles, MethodType}

import parsley.errors.ErrorBuilder

import parsley.internal.machine.{Context, ParseRunner}
import parsley.internal.machine.jit.{Continuation, JitContext}

import parsley.{Failure, Result, Success}

class JitParseRunner(functions: Array[ParserFunction], numRegs: Int, fallback: () => ParseRunner) extends ParseRunner {
    private val lookup = MethodHandles.lookup()
    private val implClass = new ParserGenerator(functions).generate()
    private val parseMethod = lookup.findStatic(
        implClass,
        FunctionGenerator.Constants.IMPL_NAME,
        MethodType.fromMethodDescriptorString(FunctionGenerator.implDesc(true), getClass.getClassLoader)
    )
    private val startMethod = if (!functions.head.isCyclic) null else lookup.findStatic(
        implClass,
        StateMachineFunctionGenerator.Constants.START_NAME,
        MethodType.fromMethodDescriptorString(StateMachineFunctionGenerator.Constants.START_DESC, getClass.getClassLoader)
    )

    override def run[Err: ErrorBuilder, A](input: String, sourceFile: Option[String]): Result[Err, A] =
        new JitContext(parseMethod, input, numRegs, sourceFile).run() match {
            case success: Success[?] => success
            case Failure(_) =>
                System.err.println("Falling back to interpreter")
                fallback().run(input, sourceFile)
        }

    override def dynCall(ctx: Context, pc: Int, continuation: AnyRef): Continuation =
        ctx match {
            case ctx: JitContext =>
                if (startMethod ne null) {
                    startMethod.invokeExact(continuation.asInstanceOf[Continuation], ctx)
                } else {
                    // Inner parser is not a state machine
                    new Continuation(continuation.asInstanceOf[Continuation]) {
                        override def step(ctx: JitContext): Continuation = {
                            returnWith(parseMethod.invokeExact(ctx))
                        }
                    }
                }
        }
}
