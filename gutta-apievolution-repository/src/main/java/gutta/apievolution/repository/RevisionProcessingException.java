package gutta.apievolution.repository;

/**
 * Denotes that an error has occurred while processing a revision.
 */
public class RevisionProcessingException extends RuntimeException {

    /**
     * Creates a new exception with the given data.
     * @param message The message for this exception
     */
    public RevisionProcessingException(String message) {
        super(message);
    }

    /**
     * Creates a new exception with the given data.
     * @param message The message for this exception
     * @param cause The cause for this exception
     */
    public RevisionProcessingException(String message, Throwable cause) {
        super(message, cause);
    }

}
