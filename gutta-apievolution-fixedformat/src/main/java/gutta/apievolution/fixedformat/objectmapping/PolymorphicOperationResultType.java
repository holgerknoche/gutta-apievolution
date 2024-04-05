package gutta.apievolution.fixedformat.objectmapping;

import java.util.Set;

class PolymorphicOperationResultType<T> extends OperationResultType<T> {
    
    private final Set<Class<?>> exceptionTypes;
    
    public PolymorphicOperationResultType(Class<T> resultType, Set<Class<?>> exceptionTypes) {
        super(resultType);
        
        this.exceptionTypes = exceptionTypes;
    }
    
    @Override
    public boolean isPolymorphic() {
        return true;
    }
    
    @Override
    public Set<Class<?>> getExceptionTypes() {
        return this.exceptionTypes;
    }

}
