package gutta.apievolution.fixedformat.objectmapping;

import static java.util.Objects.*;

public class ValueOrException<T> {
    
    private final T value;
    
    private final Object exception;

    public static <T> ValueOrException<T> forNull() {
        return new ValueOrException<T>(null, null);
    }
    
    public static <T> ValueOrException<T> forValue(T value) {
        return new ValueOrException<>(requireNonNull(value), null);
    }
    
    public static ValueOrException<?> forException(Object exception) {
        return new ValueOrException<>(null, requireNonNull(exception));
    }
    
    private ValueOrException(T value, Object exception) {
        this.value = value;
        this.exception = exception;
    }
    
    public boolean containsException() {
        return (this.exception != null);
    }

    public T getValue() {
        return this.value;
    }
    
    public Object getException() {
        return this.exception;
    }
    
}
