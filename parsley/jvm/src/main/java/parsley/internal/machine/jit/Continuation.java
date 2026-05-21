package parsley.internal.machine.jit;

public abstract class Continuation {
    public final Continuation next;

    public Object result;

    public Continuation(Continuation next) {
        this.next = next;
    }

    public abstract Continuation step(JitContext ctx);

    public static Object run(Continuation c, JitContext ctx) {
        Continuation resultHolder = ctx.resultHolder();
        while (c != resultHolder) {
            c = c.step(ctx);
        }
        return resultHolder.result;
    }

    protected final Continuation returnWith(Object result) {
        Continuation next = this.next;
        next.result = result;
        return next;
    }
}
