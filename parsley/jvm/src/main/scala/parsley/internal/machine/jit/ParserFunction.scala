package parsley.internal.machine.jit

import parsley.internal.machine.instructions.Instr

private[jit] class ParserFunction(val id: Int, val instrs: Array[Instr], val producesResults: Boolean, val info: FunctionInfo)
