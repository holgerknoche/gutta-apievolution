package gutta.apievolution.inprocess;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * This annotation is used to define the API name that is represented by an API implementation class.
 */
@Documented
@Retention(RUNTIME)
@Target(TYPE)
public @interface ApiName {

    /**
     * Returns the name of the API implemented by the annotated class.
     * 
     * @return see above
     */
    String value();

}
