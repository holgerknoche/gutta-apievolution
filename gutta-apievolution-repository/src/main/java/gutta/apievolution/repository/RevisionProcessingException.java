package gutta.apievolution.repository;

public class RevisionProcessingException extends RuntimeException {

    public RevisionProcessingException(String message) {
        super(message);
    }

    public RevisionProcessingException(String message, Throwable cause) {
        super(message, cause);
    }

}
