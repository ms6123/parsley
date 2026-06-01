package parsley.internal.machine.jit;

import parsley.internal.machine.ParseRunner;
import parsley.internal.machine.jit.codegen.ClassGenContext$;
import scala.Function3;

@SuppressWarnings("unused")
public class JitRuntime {
    public static Object getObject(Class<?> ctx, String name, int index) {
        return ClassGenContext$.MODULE$.getObject(index, ctx);
    }
    
    public static void beforeInstruction(Object instr, int i, Object ctx) {
        System.err.println(instr);
    }
    
    public static void debug(String str) {
        System.err.println(str);
    }
    
    public static Continuation dynCall(Object x, Continuation caller, JitContext ctx, Function3<Object, Integer, Boolean, ParseRunner> f) {
        ParseRunner runner = f.apply(x, ctx.regs().length, true);
        return (Continuation) runner.dynCall(ctx, -1, caller);
    }
}
