package gutta.apievolution.core.apimodel;

/**
 * This exception indicates that an attempt was made to create an invalid API
 * definition, for instance, adding two supertypes to a record type.
 */
public class InvalidApiDefinitionException extends RuntimeException {

    private static final long serialVersionUID = 3948698901439917077L;

    InvalidApiDefinitionException(String message) {
        super(message);
    }

}
