/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
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
