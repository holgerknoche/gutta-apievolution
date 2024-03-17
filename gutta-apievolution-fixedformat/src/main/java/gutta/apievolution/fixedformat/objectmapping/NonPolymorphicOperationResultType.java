package gutta.apievolution.fixedformat.objectmapping;

import java.util.Collections;
import java.util.Set;

class NonPolymorphicOperationResultType<T> extends OperationResultType<T> {

    public NonPolymorphicOperationResultType(Class<T> resultType) {
        super(resultType);
    }
    
    @Override
    public boolean isPolymorphic() {
        return false;
    }
    
    @Override
    public Set<Class<?>> getExceptionTypes() {
        return Collections.emptySet();
    }

}
