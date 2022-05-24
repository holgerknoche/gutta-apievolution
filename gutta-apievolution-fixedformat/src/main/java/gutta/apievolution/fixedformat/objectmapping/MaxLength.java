package gutta.apievolution.fixedformat.objectmapping;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * Annotation to specify the maximum length of an element, such as a field.
 */
@Retention(RUNTIME)
@Target(FIELD)
public @interface MaxLength {

    /**
     * Returns the maximum length in the given unit, such as bytes or list elements.
     * 
     * @return see above
     */
    public int value();
    
}
