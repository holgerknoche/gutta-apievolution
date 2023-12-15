package gutta.apievolution.json;

/**
 * Exception denoting that an invocation has failed due to another exception.
 */
public class InvocationFailedException extends RuntimeException {

    private static final long serialVersionUID = -4471374766947528712L;

    InvocationFailedException(Throwable cause) {
        super(cause);
    }

}
