package gutta.apievolution.fixedformat.objectmapping;

import static java.util.Objects.*;

/**
 * This class represents a result that may be a regular value or an exception.
 * 
 * @param <T> The type of the regular value, if present
 */
public class ValueOrException<T> {
    
    private final T value;
    
    private final Object exception;

    /**
     * Returns an appropriate object for returning {@code null} as a regular value.
     * 
     * @param <T> The type of the regular value
     * @return The result object
     */
    public static <T> ValueOrException<T> forNull() {
        return new ValueOrException<T>(null, null);
    }
    
    /**
     * Returns an appropriate object for returning a non-{@code null} regular value.
     * 
     * @param <T> The type of the regular value
     * @param value The value to return
     * @return The result object
     */
    public static <T> ValueOrException<T> forValue(T value) {
        return new ValueOrException<>(requireNonNull(value), null);
    }
    
    /**
     * Returns an appropriate object for returning the given exception data.
     * 
     * @param exception The exception object to return
     * @return The result object
     */
    public static ValueOrException<?> forException(Object exception) {
        return new ValueOrException<>(null, requireNonNull(exception));
    }
    
    private ValueOrException(T value, Object exception) {
        this.value = value;
        this.exception = exception;
    }
    
    /**
     * Denotes whether this object contains an exception.
     * 
     * @return see above
     */
    public boolean containsException() {
        return (this.exception != null);
    }

    /**
     * Returns the result object, if present. Note that this is only the case if {@link #containsException()} returns {@code false}.
     * 
     * @return The result object
     */
    public T getValue() {
        return this.value;
    }
    
    /**
     * Returns the exception object, if present. Note that this is only the case if {@link #containsException()} returns {@code false}.
     * 
     * @return The result object
     */
    public Object getException() {
        return this.exception;
    }
    
}
