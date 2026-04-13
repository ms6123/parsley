package parsley.internal.machine.jit;

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
}
