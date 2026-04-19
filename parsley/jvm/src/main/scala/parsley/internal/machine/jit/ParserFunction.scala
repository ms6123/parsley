package parsley.internal.machine.jit

import parsley.internal.machine.instructions.Instr

private[jit] class ParserFunction(instrs: Array[Instr], successorInfos: Array[SuccessorInfo])
