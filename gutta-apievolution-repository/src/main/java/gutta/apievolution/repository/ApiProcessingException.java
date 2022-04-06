package gutta.apievolution.repository;

/**
 * This exception denotes that an error has occurred while processing an API
 * definition.
 */
public class ApiProcessingException
        extends RuntimeException {

    ApiProcessingException(String message) {
        super(message);
    }

    ApiProcessingException(String message, Throwable cause) {
        super(message, cause);
    }

    @Override
    public synchronized Throwable fillInStackTrace() {
        // Suppress stack trace
        return this;
    }

}
