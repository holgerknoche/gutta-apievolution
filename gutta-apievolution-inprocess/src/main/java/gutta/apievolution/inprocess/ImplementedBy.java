package gutta.apievolution.inprocess;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * Denotes the concrete class that implements an abstract type for the purpose of object mapping.
 */
@Documented
@Retention(RUNTIME)
@Target(TYPE)
public @interface ImplementedBy {

    /**
     * The class that implements the annotated type. 
     * 
     * @return see above
     */
    Class<?> value();

}
