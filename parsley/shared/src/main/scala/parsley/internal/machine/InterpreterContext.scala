package parsley.internal.machine

import parsley.internal.machine.instructions.Instr
import parsley.internal.machine.stacks.Stack.StackExt

private[machine] class InterpreterContext(private[this] val startInstrs: Array[Instr],
                                          input: String,
                                          numRegs: Int,
                                          sourceFile: Option[String]) extends Context(input, numRegs, sourceFile) {
    override protected def runImpl(): Unit = {
        instrs = startInstrs
        while (running) {
            instrs(pc)(this)
        }
    }

    override def failImpl(): Unit = {
        if (handlers.isEmpty) {
            running = false
        }
        else {
            val handler = handlers
            instrs = handler.instrs
            calls = handler.calls
            pc = handler.pc
            val diffstack = stack.usize - handler.stacksz
            if (diffstack > 0) stack.drop(diffstack)
        }
    }
}
