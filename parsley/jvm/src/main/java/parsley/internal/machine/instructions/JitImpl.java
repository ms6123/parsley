package parsley.internal.machine.instructions;

import scala.runtime.Nothing$;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface JitImpl {
    int consumeOperands() default 0;

    Class<?> fallthroughMarker() default Nothing$.class;
    
    Action[] beforeActions() default {};
    
    Action[] afterActions() default {};
    
    enum Action {
        PushTrue,
        Swap,
        DupX1
    }
}
