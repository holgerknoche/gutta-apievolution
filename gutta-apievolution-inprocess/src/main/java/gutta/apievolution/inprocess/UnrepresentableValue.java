package gutta.apievolution.inprocess;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * This annotation is used to mark enum members or methods as (providers of) representations of unrepresentable values. If a method is annotated with
 * this annotation, it is expected to return a default value (possibly {@code null}) or throw an unchecked exception.
 */
@Documented
@Retention(RUNTIME)
@Target({METHOD, FIELD})
public @interface UnrepresentableValue {

}
