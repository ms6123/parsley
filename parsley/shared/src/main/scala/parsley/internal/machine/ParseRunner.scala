package parsley.internal.machine

private[parsley] trait ParseRunner {
    type ContextT <: Context
    
    def newContext(input: String, numRegs: Int, sourceFile: Option[String]): ContextT
    
    def dynCall(ctx: ContextT): Unit
}
