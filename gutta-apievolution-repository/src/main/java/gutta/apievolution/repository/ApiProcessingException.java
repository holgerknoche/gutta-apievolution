package gutta.apievolution.repository;

public class ApiProcessingException extends RuntimeException {

    public ApiProcessingException(String message, Throwable cause) {
        super(message, cause);
    }

    @Override
    public synchronized Throwable fillInStackTrace() {
        // Suppress stack trace
        return this;
    }

}
