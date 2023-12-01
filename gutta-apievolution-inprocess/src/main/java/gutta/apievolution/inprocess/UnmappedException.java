package gutta.apievolution.inprocess;

/**
 * This exception denotes that an unmapped exception has occurred during the invocation of an API method.
 */
public class UnmappedException extends RuntimeException {

    private static final long serialVersionUID = 6494415670059682176L;

    /**
     * Creates a new exception with the given cause.
     * 
     * @param cause The unmapped exception that was thrown
     */
    public UnmappedException(Throwable cause) {
        super("An unmapped exception was thrown during an API invocation.", cause);
    }

    @Override
    public synchronized Throwable fillInStackTrace() {
        // No stack trace, as it is only confusing
        return this;
    }

}
