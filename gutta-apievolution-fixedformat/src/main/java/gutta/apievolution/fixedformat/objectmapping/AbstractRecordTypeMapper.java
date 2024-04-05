package gutta.apievolution.fixedformat.objectmapping;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

abstract class AbstractRecordTypeMapper extends UserDefinedTypeMapper {
    
    protected static final int DISCRIMINATOR_SIZE = 4;
    
    private final Class<?> formalResultType;
    
    protected AbstractRecordTypeMapper(Class<?> formalResultType) {
        this.formalResultType = formalResultType;
    }
        
    protected int determineTypeIdFor(Object value) {
        Class<?> type = value.getClass();
        TypeId typeIdAnnotation = type.getAnnotation(TypeId.class);
        if (typeIdAnnotation == null) {
            throw new InvalidRepresentationElementException("Missing type id on type " + type + ".");
        }

        return typeIdAnnotation.value();
    }
    
    private Method findUnrepresentableValueHandler() {
        for (Method method : this.formalResultType.getMethods()) {
            if (method.isAnnotationPresent(UnrepresentableValue.class)) {
                return method;
            }
        }
        
        return null;
    }
    
    @Override
    protected Object handleUnrepresentableValue() {
        Method unrepresentableValueHandler = this.findUnrepresentableValueHandler();
        if (unrepresentableValueHandler != null) {
            try {
                return unrepresentableValueHandler.invoke(null);
            } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
                throw new RuntimeException("An error occurred invoking the unrepresentable value handler on class '" + this.formalResultType + "'.", e);
            }
        } else {
            throw new UnrepresentableValueException("An unrepresentable subtype of '" + this.formalResultType +
                    "' was encountered, and no handler was defined.");
        }
    }

}
