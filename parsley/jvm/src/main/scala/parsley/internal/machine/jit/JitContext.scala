package parsley.internal.machine.jit

import java.lang.invoke.MethodHandle

import parsley.internal.machine.Context
import parsley.internal.machine.stacks.Stack.StackExt

private[jit] class JitContext(private val startMethod: MethodHandle,
                              input: String,
                              numRegs: Int,
                              sourceFile: Option[String]) extends Context(input, numRegs, sourceFile) {
    override protected def runImpl(): Unit =
        startMethod.invokeExact(this)

    override protected def failImpl(): Unit = {
        if (handlers.isEmpty) {
            running = false
            pc = -1
        } else {
            val handler = handlers
            if (calls eq handler.calls) {
                // Local handler
                pc = handler.pc
            } else {
                // Handler in parent method
                pc = -1
            }
            calls = handler.calls
            val diffstack = stack.usize - handler.stacksz
            if (diffstack > 0) stack.drop(diffstack)
        }
    }
}
