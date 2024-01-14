package gutta.apievolution.fixedformat.objectmapping;

/**
 * This type of exception is thrown when an invalid representation element is encountered, e.g.,
 * a type or a field is missing a required annotation.
 */
public class InvalidRepresentationElementException extends RuntimeException {
    
    private static final long serialVersionUID = 9027533289607889400L;

    InvalidRepresentationElementException(String message) {
        super(message);
    }

}
