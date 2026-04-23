package parsley.internal.machine

private[parsley] trait ParseRunner {
    def run(ctx: Context): Unit

    def dynCall(ctx: Context): Unit
    
    def fail(ctx: Context): Unit
}
