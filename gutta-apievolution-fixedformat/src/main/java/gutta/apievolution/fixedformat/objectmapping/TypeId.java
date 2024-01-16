package gutta.apievolution.fixedformat.objectmapping;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * This annotation is used to specify the type id in the fixed data format on a
 * representation type.
 */
@Retention(RUNTIME)
@Target(TYPE)
public @interface TypeId {
    
    /**
     * The type id of the annotated type.
     * 
     * @return see above
     */
    int value();

}
