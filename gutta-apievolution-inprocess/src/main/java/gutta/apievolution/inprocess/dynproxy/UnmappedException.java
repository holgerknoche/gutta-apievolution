package gutta.apievolution.inprocess.dynproxy;

public class UnmappedException extends RuntimeException {

    private static final long serialVersionUID = 6494415670059682176L;

    UnmappedException(Throwable cause) {
        super("An unmapped exception was thrown during an API invocation.", cause);
    }
    
    @Override
    public synchronized Throwable fillInStackTrace() {
        // No stack trace, as it is only confusing
        return this;
    }
    
}
