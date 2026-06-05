/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface JitImpl {
    boolean noop() default false;
    
    int consumeOperands() default 0;

    IntKind intReturnKind() default IntKind.Pc;
    
    Action[] beforeActions() default {};
    
    Action[] afterActions() default {};

    String[] constants() default {};

    Param[] params() default {};

    int[] updateCheckOffsets() default {};
    
    enum Action {
        PushTrue,
        Swap,
        DupX1,
        Dup
    }

    enum Param {
        Pc,
        HandlerCheck
    }

    enum IntKind {
        Pc,
        Char,
        CodePoint
    }
}
