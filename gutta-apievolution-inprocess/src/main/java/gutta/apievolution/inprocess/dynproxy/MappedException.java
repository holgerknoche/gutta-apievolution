package gutta.apievolution.inprocess.dynproxy;

import java.util.Optional;

/**
 * This exception serves as a container for transporting mapped exception data to the consumer as part of the dynamic proxy mapping strategy. As dynamic proxies
 * must be interfaces and exceptions must be classes, no such proxies can be created for exceptions, which created the necessity for this exception.
 */
public class MappedException extends RuntimeException {

    private static final long serialVersionUID = 1054540723344482824L;

    private final Object exceptionData;

    MappedException(Object exceptionData) {
        this.exceptionData = exceptionData;
    }

    @Override
    public synchronized Throwable fillInStackTrace() {
        // No stack trace, as it is only confusing
        return this;
    }

    /**
     * Returns the mapped exception data as the given type, if possible.
     * 
     * @param <T>  The type of the expected data
     * @param type The desired type of the data
     * @return The data as the desired type, or no data if the type of the actual data does not match
     */
    @SuppressWarnings("unchecked")
    public <T> Optional<T> getDataAs(Class<T> type) {
        Class<?> actualType = this.exceptionData.getClass();
        if (!type.isAssignableFrom(actualType)) {
            return Optional.empty();
        } else {
            return Optional.of((T) this.exceptionData);
        }
    }

}
