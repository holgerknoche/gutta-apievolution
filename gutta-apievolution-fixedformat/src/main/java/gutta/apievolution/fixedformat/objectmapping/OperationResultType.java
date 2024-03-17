package gutta.apievolution.fixedformat.objectmapping;

import java.util.Set;

public abstract class OperationResultType<T> {
    
    private final Class<T> resultType;
    
    public static <X> OperationResultType<X> of(Class<X> resultType, Set<Class<?>> exceptionTypes) {
        if (exceptionTypes == null || exceptionTypes.isEmpty()) {
            return new NonPolymorphicOperationResultType<>(resultType);
        } else {
            return new PolymorphicOperationResultType<>(resultType, exceptionTypes);
        }
    }
    
    protected OperationResultType(Class<T> resultType) {
        this.resultType = resultType;
    }
    
    public Class<T> getResultType() {
        return resultType;
    }
    
    public abstract Set<Class<?>> getExceptionTypes();
        
    public abstract boolean isPolymorphic();        

}
