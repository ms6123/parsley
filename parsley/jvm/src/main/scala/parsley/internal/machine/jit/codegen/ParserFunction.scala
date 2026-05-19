package parsley.internal.machine.jit.codegen

import parsley.internal.machine.instructions.Instr
import parsley.internal.machine.jit.FunctionInfo

private[jit] class ParserFunction(val id: Int, val instrs: Array[Instr], val producesResults: Boolean, val info: FunctionInfo) {
    val needsStateMachine = true
}
