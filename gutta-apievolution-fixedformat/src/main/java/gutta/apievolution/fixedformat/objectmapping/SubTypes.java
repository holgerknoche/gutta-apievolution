package gutta.apievolution.fixedformat.objectmapping;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * This annotation is used to identify polymorphic types and explicitly define their subtypes.
 */
@Retention(RUNTIME)
@Target(TYPE)
public @interface SubTypes {

    /**
     * Returns the subtypes of the annotated type.
     *  
     * @return see above
     */
    Class<?>[] value(); 
    
}
