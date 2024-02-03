package gutta.apievolution.fixedformat.objectmapping;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * This annotation is used to denote members in enums or methods in polymorphic records
 * that is used or invoked if an unrepresentable member or subtype is encountered.
 */
@Retention(RUNTIME)
@Target({ FIELD, METHOD })
public @interface UnrepresentableValue {

}
