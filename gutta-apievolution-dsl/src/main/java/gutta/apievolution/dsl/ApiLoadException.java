package gutta.apievolution.dsl;

/**
 * This exception is thrown when an error (such as an I/O error) occurs while loading an API definition.
 */
public class ApiLoadException extends RuntimeException {

    /**
     * Creates a new exception using the given data.
     * @param message The exception message
     * @param cause The cause of the exception
     */
    public ApiLoadException(String message, Throwable cause) {
        super(message, cause);
    }

}
