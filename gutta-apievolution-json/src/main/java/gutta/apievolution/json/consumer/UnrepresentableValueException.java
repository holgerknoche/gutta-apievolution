package gutta.apievolution.json.consumer;

/**
 * This exception is thrown by default if an unrepresentable value is encountered in the JSON.
 */
public class UnrepresentableValueException extends RuntimeException {
    
    private static final long serialVersionUID = 4985373813354358402L;

    UnrepresentableValueException(String message) {
        super(message);
    }

}
