package parsley.internal.machine.jit;

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
}
