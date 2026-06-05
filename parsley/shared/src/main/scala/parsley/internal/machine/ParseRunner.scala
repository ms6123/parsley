package parsley.internal.machine

import parsley.Result
import parsley.errors.ErrorBuilder

private[parsley] trait ParseRunner {
    def run[Err: ErrorBuilder, A](input: String, sourceFile: Option[String]): Result[Err, A]

    def dynCall(ctx: Context, pc: Int, continuation: AnyRef): Any
}
