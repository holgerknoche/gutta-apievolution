package gutta.apievolution.core.apimodel;

/**
 * This exception indicates that an attempt was made to create an invalid API definition,
 * for instance, adding two supertypes to a record type.
 */
public class InvalidApiDefinitionException extends RuntimeException {

    InvalidApiDefinitionException(String message) {
        super(message);
    }

}
