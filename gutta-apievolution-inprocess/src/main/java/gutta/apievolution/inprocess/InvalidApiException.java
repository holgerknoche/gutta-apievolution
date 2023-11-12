package gutta.apievolution.inprocess;

public class InvalidApiException extends RuntimeException {

    private static final long serialVersionUID = -6182617804631565134L;

    public InvalidApiException(String message) {
        super(message);
    }

    public InvalidApiException(String message, Throwable cause) {
        super(message, cause);
    }
    
}
