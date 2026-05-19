package parsley.internal.machine.jit;

public final class ContinuationResult extends Continuation {
    public ContinuationResult() {
        super(null);
    }

    @Override
    public Continuation step(JitContext ctx) {
        throw new UnsupportedOperationException();
    }
}
