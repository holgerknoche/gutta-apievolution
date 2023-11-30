package gutta.apievolution.inprocess.dynproxy;

import java.util.Optional;

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
