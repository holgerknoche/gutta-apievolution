package gutta.apievolution.inprocess;

public class InvalidInvocationException extends RuntimeException {
    
    private static final long serialVersionUID = 5903893641519255033L;

    public InvalidInvocationException(String message) {
        super(message);
    }
    
    public InvalidInvocationException(String message, Throwable cause) {
        super(message, cause);
    }

}
