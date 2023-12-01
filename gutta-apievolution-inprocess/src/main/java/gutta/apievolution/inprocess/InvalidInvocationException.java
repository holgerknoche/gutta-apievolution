package gutta.apievolution.inprocess;

/**
 * This exception denotes that an error has occured during the invocation of an API method.
 */
public class InvalidInvocationException extends RuntimeException {

    private static final long serialVersionUID = 5903893641519255033L;

    /**
     * Creates a new exception with the given message.
     * 
     * @param message The message of the exception
     */
    public InvalidInvocationException(String message) {
        super(message);
    }

    /**
     * Creates a new exception with the given message and cause.
     * 
     * @param message The message of the exception
     * @param cause   The cause of this exception
     */
    public InvalidInvocationException(String message, Throwable cause) {
        super(message, cause);
    }

}
