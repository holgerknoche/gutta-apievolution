package gutta.apievolution.core.apimodel.provider;

/**
 * This exception is thrown if an inconsistent revision history is detected.
 */
public class InconsistentHistoryException extends RuntimeException {

    InconsistentHistoryException(String message) {
        super(message);
    }

}
