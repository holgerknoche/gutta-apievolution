package gutta.apievolution.core.apimodel.provider;

/**
 * This exception is thrown if an inconsistent revision history is detected.
 */
public class InconsistentHistoryException extends RuntimeException {

    private static final long serialVersionUID = 2015705952085090467L;

    InconsistentHistoryException(String message) {
        super(message);
    }

}
