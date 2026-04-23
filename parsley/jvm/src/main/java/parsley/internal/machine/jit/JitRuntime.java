package parsley.internal.machine.jit;

import parsley.internal.machine.Context;

import java.lang.invoke.CallSite;
import java.lang.invoke.ConstantCallSite;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;

@SuppressWarnings("unused")
public class JitRuntime {
    public static CallSite getObject(MethodHandles.Lookup ctx, String name, MethodType type, int index) {
        Object value = ClassGenContext$.MODULE$.getObject(index, ctx.lookupClass());
        return new ConstantCallSite(MethodHandles.constant(type.returnType(), value));
    }
    
    public static void beforeInstruction(int i, Object ctx, Object instr) {
        if (((Context) ctx).pc() != i) {
            throw new IllegalStateException("pc mismatch");
        }
    }
}
