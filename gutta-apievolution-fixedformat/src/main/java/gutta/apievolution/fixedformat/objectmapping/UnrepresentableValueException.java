package gutta.apievolution.fixedformat.objectmapping;

/**
 * This exception is thrown by default if an unrepresentable value is encountered and
 * no specific behavior is specified.
 */
public class UnrepresentableValueException extends RuntimeException {
    
    private static final long serialVersionUID = -1679655029133082362L;

    UnrepresentableValueException(String message) {
        super(message);
    }

}
