package gutta.apievolution.fixedformat.objectmapping;

import java.util.Set;

/**
 * Supertype for operation result types, which may be implicitly polymorphic if the given operation may throw exceptions. 
 * 
 * @param <T> The type of the regular result, i.e., the value returned when the operation terminates normally
 */
public abstract class OperationResultType<T> {
    
    private final Class<T> resultType;
    
    /**
     * Returns the type representing the given result type, together with the given set of exception types.
     * 
     * @param <X> The type of the regular result
     * @param resultType The result type (which may itself be polymorphic)
     * @param exceptionTypes The (potentially empty) set of exception types
     * @return The type representing the given types
     */
    public static <X> OperationResultType<X> of(Class<X> resultType, Set<Class<?>> exceptionTypes) {
        if (exceptionTypes == null || exceptionTypes.isEmpty()) {
            return new NonPolymorphicOperationResultType<>(resultType);
        } else {
            return new PolymorphicOperationResultType<>(resultType, exceptionTypes);
        }
    }
    
    /**
     * Creates a new result type using the given data.
     * 
     * @param resultType The result type of the associated operation
     */
    protected OperationResultType(Class<T> resultType) {
        this.resultType = resultType;
    }
    
    /**
     * Returns the result type of the associated operation. 
     * 
     * @return see above
     */
    public Class<T> getResultType() {
        return resultType;
    }
    
    /**
     * Returns the exception types thrown by the associated operation.
     * 
     * @return see above
     */
    public abstract Set<Class<?>> getExceptionTypes();
        
    /**
     * Returns whether this type is (implicitly) polymorphic, i.e., may return a regular value or an exception. 
     * 
     * @return see above
     */
    public abstract boolean isPolymorphic();        

}
