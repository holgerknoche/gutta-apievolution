package gutta.apievolution.inprocess;

/**
 * This exception is thrown if an invalid API representation is encountered, such as a missing method or an implementation type that does not meet the
 * requirements of the employed mapping strategy.
 */
public class InvalidApiException extends RuntimeException {

    private static final long serialVersionUID = -6182617804631565134L;

    /**
     * Creates a new exception with the given message.
     * 
     * @param message The message of the exception
     */
    public InvalidApiException(String message) {
        super(message);
    }

    /**
     * Creates a new exception with the given message and cause.
     * 
     * @param message The message of the exception
     * @param cause   The cause of this exception
     */
    public InvalidApiException(String message, Throwable cause) {
        super(message, cause);
    }

}
