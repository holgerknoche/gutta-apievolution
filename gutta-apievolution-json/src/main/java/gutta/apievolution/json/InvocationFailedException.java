package gutta.apievolution.json;

/**
 * Exception denoting that an invocation has failed due to another exception.
 */
public class InvocationFailedException extends RuntimeException {

    InvocationFailedException(Throwable cause) {
        super(cause);
    }

}
